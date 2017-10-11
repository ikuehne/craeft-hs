{-|
Module      : Codegen
Description : Codegen from the typed AST (@TAST@) to LLVM.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

Codegen from the @TAST@ to the llvm (as in @llvm-hs-pure@) AST.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Codegen where

import qualified AST
import qualified Environment as Env
import Utility

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State (StateT)
import Data.List (lookup)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Control.Lens
import Control.Lens.Internal.Zoom ( Focusing )
import LLVM.AST
import Text.Parsec.Pos
import qualified LLVM.AST.Type as LLTy
import qualified LLVM.AST.Instruction as LLInstr
import qualified LLVM.AST.Constant as LLConst
import qualified LLVM.AST.Float as LLFloat
import qualified LLVM.AST.CallingConvention as CConv
import LLVM.AST.AddrSpace ( AddrSpace (..) )

data CodegenState = CodegenState { _currentBlock :: Name
                                 , _blocks       :: Map.Map Name BlockState
                                 , _symtab       :: Env.EnvironmentState
                                 , _blockCount   :: Int
                                 , _count        :: Word
                                 , _names        :: Map.Map String Int }

data BlockState
  = BlockState { _idx   :: Int
               , _stack :: [Named Instruction]
               , _term  :: Maybe (Named Terminator) }

makeLenses ''BlockState
makeLenses ''CodegenState

type Codegen a = StateT CodegenState CraeftExcept a

--
-- Generating names.
--

recordName :: String -> Int -> Codegen Name
recordName n i = names %= Map.insert n i >> return (mkName $ n ++ show i)

uniqueName :: String -> Codegen Name
uniqueName n = uses names (Map.lookup n) >>= recordName n . fromMaybe 0

fresh :: Codegen Word
fresh = uses count succ

current :: Codegen BlockState
current = do c <- use currentBlock
             bs <- use blocks
             case Map.lookup c bs of
               Just x -> return x
               Nothing -> throwC . InternalError $ "No such block: " ++ show c

modifyBlock :: (BlockState -> BlockState) -> Codegen ()
modifyBlock f = do active <- use currentBlock
                   blocks %= Map.adjust f active

instr :: Instruction -> Type -> Codegen Operand
instr ins t = do ref <- UnName <$> fresh
                 modifyBlock (stack %~ ((ref := ins) :))
                 return $ LocalReference t ref

--
-- Expression codegen.
--

checkPtr :: SourcePos -> Env.Value -> Codegen Env.Value
checkPtr _ v@(Env.Value (Env.Pointer _) _) = return v
checkPtr p _ = throwC $ TypeError "cannot dereference non-pointer" p

constInt :: Integer -> Operand
constInt i = ConstantOperand (LLConst.Int 64 i)

lvalueCodegen :: Annotated AST.LValue -> Codegen Env.Value
lvalueCodegen a = case contents a of
    AST.Variable s -> zoom symtab $ Env.lookupValue s p
    AST.Dereference ptr -> exprCodegen ptr >>= checkPtr p
    AST.FieldAccess str name -> do
        val <- case contents str of
            AST.LValueExpr l -> lvalueCodegen (Annotated l $ pos str)
            _ -> throwC $ TypeError "cannot assign to r-value struct" p
        case Env.ty val of
            Env.Pointer (Env.Struct fields) -> do
              (t, i) <- fromMaybe (throwC $ NameError "no such field" p)
                                  (return <$> lookupWithIndex name fields)
              let gep = GetElementPtr False (Env.value val) [constInt i] []
              llt <- typeTranslation p $ Env.Pointer t
              op <- instr gep llt
              return $ Env.Value (Env.Pointer t) op
            _ -> throwC $ TypeError "cannot look up field in non-sruct type" p
  where lookupWithIndex n assoc =
              lookup n $ (\((a, b), c) -> (a, (b, c))) <$> zip assoc [0..]
        p = pos a
                     
exprCodegen :: Annotated AST.Expression -> Codegen Env.Value
exprCodegen a = case contents a of
    AST.IntLiteral i -> let t = Env.Signed 64
                         in return $ Env.Value t (constInt i)
    AST.UIntLiteral i -> let t = Env.Unsigned 64
                         in return $ Env.Value t (constInt i)
    AST.FloatLiteral f -> let t = Env.Floating Env.DoublePrec
                              d = LLFloat.Double f
                              o = ConstantOperand (LLConst.Float d)
                           in return $ Env.Value t o
    AST.StringLiteral s -> let t = Env.Pointer (Env.Unsigned 8)
                               cs = LLConst.Int 8 . toInteger . fromEnum <$> s
                            in do llt <- typeTranslation p t
                                  let c = LLConst.Array llt cs
                                      o = ConstantOperand c
                                  return $ Env.Value t o
    AST.Reference l -> lvalueCodegen l
    AST.Binop l op r -> do
        lhs <- exprCodegen l
        rhs <- exprCodegen r
        let msg = "no such op: " ++ op
        f <- fromMaybe (throwC $ TypeError msg p)
                       (return <$> Map.lookup op ops)
        f p lhs rhs
    AST.FunctionCall f args -> do
        func' <- exprCodegen f
        let func = Right $ Env.value func'

        retty <- case Env.ty func' of
          Env.Function _ t -> return t
          _ -> throwC $ TypeError "cannot call non-function" p

        args'' <- mapM exprCodegen args
        let args' = [(Env.value a, []) | a <- args'']
        let inst = LLInstr.Call Nothing CConv.C [] func args' [] []
        llretty <- typeTranslation p retty
        Env.Value retty <$> instr inst llretty
    _ -> undefined
  where p = pos a
        ops = Map.fromList [("+", addValues)]

addValues :: SourcePos -> Env.Value -> Env.Value -> Codegen Env.Value
addValues p l r = case (Env.ty l, Env.ty r) of
  (Env.Signed lb, Env.Signed rb) ->
      let resulty = Env.Signed (max lb rb)
       in do llty <- typeTranslation p resulty
             Env.Value resulty <$> instr int llty
  _ -> throwC $ TypeError "addition not defined between these types" p
  where lo = Env.value l
        ro = Env.value r
        int = LLInstr.Add False False lo ro []

--
-- Type codegen.
--

typeTranslation :: SourcePos -> Env.Type -> Codegen Type
typeTranslation p (Env.Struct fields) = do
    let ts = map snd fields
    types <- mapM (typeTranslation p) ts
    return $ LLTy.StructureType False types
typeTranslation p (Env.Pointer t) = do
    llt <- typeTranslation p t
    return $ LLTy.PointerType llt (AddrSpace 0)
typeTranslation p (Env.Signed i) = return . LLTy.IntegerType $ fromIntegral i
typeTranslation p (Env.Unsigned i) =
    return . LLTy.IntegerType $ fromIntegral i
typeTranslation p (Env.Floating prec) = return . LLTy.FloatingPointType $ 
    case prec of Env.SinglePrec -> LLTy.FloatFP
                 Env.DoublePrec -> LLTy.DoubleFP
typeTranslation p (Env.Function ts t) = do
    args <- mapM (typeTranslation p) ts
    ret <- typeTranslation p t
    return $ LLTy.FunctionType ret args False
typeTranslation p Env.Void = return LLTy.VoidType

typeCodegen :: Annotated AST.Type -> Codegen Env.Type
typeCodegen a = case contents a of
    AST.NamedType name -> zoom symtab $ Env.lookupType name p
    AST.Void -> return Env.Void
    AST.Pointer t -> Env.Pointer <$> typeCodegen t
  where p = pos a

-- 
-- Statement codegen.
--

