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
{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import qualified TypedAST as TAST
import qualified Environment as Env
import Utility

import Debug.Trace (traceM)
import Data.Function (on)
import Data.String (fromString)
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State (StateT)
import Data.List (lookup, sortBy)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map

import Control.Lens
import Control.Lens.Internal.Zoom ( Focusing )
import LLVM.AST
import qualified LLVM.AST.Type as LLTy
import qualified LLVM.AST.Instruction as LLInstr
import qualified LLVM.AST.Constant as LLConst
import qualified LLVM.AST.Float as LLFloat
import qualified LLVM.AST.Global as LLGlobal
import qualified LLVM.AST.CallingConvention as CConv
import LLVM.AST.AddrSpace ( AddrSpace (..) )

data CodegenState = CodegenState { _currentBlock :: Name
                                 , _blocks       :: Map.Map Name BlockState
                                 , _symtab       :: Env.EnvironmentState
                                 , _blockCount   :: Int
                                 , _count        :: Word
                                 , _globals      :: [LLGlobal.Global]
                                 , _names        :: Map.Map String Int }

initCG :: Env.EnvironmentState -> CodegenState
initCG st = CodegenState { _currentBlock = Name "main"
                         , _blocks = Map.empty
                         , _symtab = st
                         , _blockCount = 0
                         , _count = 0
                         , _globals = []
                         , _names = Map.empty }

data BlockState
  = BlockState { _idx   :: Int
               , _stack :: [Named Instruction]
               , _term  :: Maybe (Named Terminator) }

data LlvmState = LlvmState { _llmod :: Module
                           , _env :: Env.EnvironmentState }

makeLenses ''BlockState
makeLenses ''CodegenState
makeLenses ''LlvmState

type Codegen a = CraeftMonad CodegenState a
type LLVM a = CraeftMonad LlvmState a

initLLVM :: LlvmState
initLLVM = LlvmState { _llmod = defaultModule
                     , _env = Env.initialEnv }
 

codegen :: TAST.Program -> CraeftExcept Module
codegen program =
    _llmod <$> execStateT (mapM toplevelCodegen program) initLLVM

--
-- Generating names.
--

recordName :: String -> Int -> Codegen Name
recordName n i = names %= Map.insert n next >> return (mkName $ n ++ show next)
  where next = i + 1

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

voidInstr :: Instruction -> Codegen ()
voidInstr ins = modifyBlock (stack %~ (Do ins :))

instr :: Instruction -> Type -> Codegen Operand
instr ins t = do ref <- UnName <$> fresh
                 modifyBlock (stack %~ ((ref := ins) :))
                 return (LocalReference t ref)

--
-- Blocks.
--

addBlock :: String -> Codegen Name
addBlock bname = do
    newBlock <- (\i -> BlockState i [] Nothing) <$> use blockCount

    newName <- uniqueName bname
    blockCount += 1
    blocks %= Map.insert newName newBlock

    return newName

terminate :: Terminator -> Codegen ()
terminate trm = do active <- use currentBlock
                   modifyBlock (term %~ const (Just $ Do trm) )

switchBlock :: Name -> Codegen ()
switchBlock = (currentBlock .=)

inBlock :: Name -> Codegen a -> Codegen a
inBlock n c = do switchBlock n
                 zoom symtab Env.push
                 ret <- c
                 zoom symtab Env.pop
                 return ret

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (_idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (_blocks m)

addCgGlobals :: CodegenState -> LLVM ()
addCgGlobals st = do
    defs <- moduleDefinitions <$> use llmod
    llmod %= \s -> s { moduleDefinitions = defs ++ newDefs }
  where newDefs = GlobalDefinition <$> _globals st

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

define ::  Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    LLGlobal.name        = mkName label
  , LLGlobal.parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , LLGlobal.returnType  = retty
  , LLGlobal.basicBlocks = body }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- moduleDefinitions <$> use llmod
  llmod %= \s -> s { moduleDefinitions = defs ++ [d] }

--
-- Expression codegen.
--

constInt :: Integer -> Operand
constInt i = ConstantOperand (LLConst.Int 64 i)

lvalueCodegen :: Env.Type -> Annotated TAST.LValue -> Codegen Env.Value
lvalueCodegen t a = case contents a of
    TAST.Variable s -> zoom symtab $ Env.lookupValue s p
    TAST.Dereference ptr referandTy ->
        let expr = TAST.Expression (contents ptr) (Env.Pointer referandTy)
         in exprCodegen (Annotated expr $ pos ptr)
    TAST.FieldAccess str members i -> do
        let structTy = Env.Struct members
        val <- case TAST.exprContents (TAST.Expression str structTy) of
            TAST.LValueExpr l -> lvalueCodegen (Env.Struct members)
                                               (Annotated l p)
            _ -> throw "cannot assign to r-value struct"
        let gep = GetElementPtr False (Env.value val) [constInt i] []
        llt <- typeTranslation p t
        op <- instr gep llt
        return $ Env.Value t op
  where p = pos a
        throw = throwC . flip TypeError p

ptr :: Type -> Type
ptr ty = PointerType ty (AddrSpace 0)
                     
exprCodegen :: Annotated TAST.Expression -> Codegen Env.Value
exprCodegen a = case TAST.exprContents $ contents a of
    TAST.IntLiteral i -> return $ Env.Value ty (constInt i)
    TAST.UIntLiteral i -> return $ Env.Value ty (constInt i)
    TAST.FloatLiteral f -> -- Construct the corresponding LLVM constant first.
                           let d = LLFloat.Double f
                               o = ConstantOperand (LLConst.Float d)
                           in return $ Env.Value ty o
    TAST.StringLiteral s -> do
        globalCount <- length <$> use globals
        llt <- typeTranslation p ty
        let globalName = mkName $ "##strLiteral" ++ show globalCount
            nullTerminated = s ++ "\0"
            cs = LLConst.Int 8 . toInteger . fromEnum <$> nullTerminated
            arrayType = LLTy.ArrayType (fromIntegral $ length nullTerminated)
                                       LLTy.i8
            globalLiteral = LLGlobal.globalVariableDefaults
              { LLGlobal.name = globalName
              , LLGlobal.isConstant = True
              , LLGlobal.type' = arrayType
              , LLGlobal.initializer = Just $
                  LLConst.Array LLTy.i8 cs }
            globalOp = LLConst.GlobalReference (ptr arrayType) globalName
            gepIdxs = [LLConst.Int 64 0, LLConst.Int 64 0]
            gep = LLConst.GetElementPtr False globalOp gepIdxs
            o = ConstantOperand gep
        globals %= (globalLiteral :)
        return $ Env.Value ty o
    TAST.Reference l -> lvalueCodegen ty (Annotated l p)
    TAST.Binop l op r -> do
        lhs <- exprCodegen l
        rhs <- exprCodegen r
        let msg = "no such op: " ++ op
        f <- fromMaybe (throwC $ TypeError msg p)
                       (return <$> Map.lookup op ops)
        f p lhs rhs
    TAST.FunctionCall f args -> do
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
    TAST.LValueExpr lv -> do
        cged <- lvalueCodegen ty (Annotated lv p)
        case Env.ty cged of
            Env.Pointer referandTy -> do
                let inst = LLInstr.Load False (Env.value cged) Nothing 0 []
                llty <- typeTranslation p ty
                Env.Value ty <$> instr inst llty
            Env.Function _ _ -> return cged
    other -> error $ show other
  where p = pos a
        ops = Map.fromList [("+", addValues)]
        ty = TAST.exprType $ contents a

addValues :: SourcePos -> Env.Value -> Env.Value -> Codegen Env.Value
addValues p l r = case (Env.ty l, Env.ty r) of
  (Env.Signed lb, Env.Signed rb) ->
      let resulty = Env.Signed (max lb rb)
       in do llty <- typeTranslation p resulty
             Env.Value resulty <$> instr int llty
  (Env.Unsigned lb, Env.Unsigned rb) ->
      let resulty = Env.Unsigned (max lb rb)
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
    return $ ptr llt
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

-- 
-- Statement codegen.
--

stmtCodegen :: Annotated TAST.Statement -> Codegen ()
stmtCodegen (Annotated stmt p) = case stmt of
    TAST.ExpressionStmt e -> void $ exprCodegen (Annotated e p)
    TAST.Return e -> do
        retval <- Env.value <$> exprCodegen e
        terminate $ LLInstr.Ret (Just retval) []
    TAST.VoidReturn -> terminate $ LLInstr.Ret Nothing []
    TAST.Assignment lhs rhs -> do
        lhsAddr <- Env.value <$> lvalueCodegen (Env.Pointer $ exprTy rhs) lhs
        rhsExpr <- Env.value <$> exprCodegen rhs
        voidInstr $ LLInstr.Store False lhsAddr rhsExpr Nothing 0 []
    TAST.Declaration n ty -> void $ declare n ty
    TAST.CompoundDeclaration n ty init -> do
        -- Declare the new variable,
        space <- declare n ty
        -- codegen the initial value,
        initVal <- Env.value <$> exprCodegen init
        -- and do the assignment.
        voidInstr $ LLInstr.Store False space initVal Nothing 0 []
    TAST.IfStatement cond ifB elseB -> do
        ifthen <- addBlock "if.then"
        ifelse <- addBlock "if.else"
        ifmerge <- addBlock "if.merge"

        test <- Env.value <$> exprCodegen cond
        terminate $ LLInstr.CondBr test ifthen ifelse []
        
        inBlock ifthen $
            mapM_ stmtCodegen ifB

        inBlock ifelse $
            mapM_ stmtCodegen elseB

        switchBlock ifmerge
  where exprTy = TAST.exprType . contents
        declare n ty = do
            llty <- typeTranslation p ty
            alloca <- instr (LLInstr.Alloca llty Nothing 0 []) (ptr llty)
            zoom symtab $ Env.insertValue n (Env.Value (Env.Pointer ty) alloca)
            return alloca
 
toplevelCodegen :: Annotated TAST.TopLevel -> LLVM ()
toplevelCodegen (Annotated tl p) = case tl of
    TAST.Function sig body -> do
        (args, retty) <- codegenSig sig
        (_, cgState) <- runCgInLlvm $ do
            fnBlock <- addBlock (TAST.name sig)
            inBlock fnBlock $ do mapM_ declareArg (TAST.args sig)
                                 mapM_ stmtCodegen body
        addCgGlobals cgState
        define retty (TAST.name sig) args (createBlocks cgState)
    TAST.FunctionDecl sig -> do
        (args, retty) <- codegenSig sig
        define retty (TAST.name sig) args []
  where runCgInLlvm :: Codegen a -> LLVM (a, CodegenState)
        runCgInLlvm cg = do
          cgState <- initCG <$> use env
          let maybeSt = runStateT cg cgState
          case runIdentity $ runExceptT maybeSt of
              Left err -> throwC err
              Right res -> return res
        codegenSig :: TAST.FunctionSignature -> LLVM ([(Type, Name)], Type)
        codegenSig (TAST.Sig name args retty) = do
            cgState <- initCG <$> use env
            (fty, llretty, argtys) <- fmap fst $ runCgInLlvm $ do
                llretty <- typeTranslation p retty
                argtys <- mapM (typeTranslation p . snd) args
                let fty = LLTy.FunctionType llretty argtys False 
                return (ptr fty, llretty, argtys)
            let op = ConstantOperand $ LLConst.GlobalReference fty $ mkName name
                val = Env.Value (Env.Function (map snd args) retty) op
            zoom env $ Env.insertValue name val
            return (zip argtys (argName . fst <$> args), llretty)
        declareArg (str, ty) = do
            llty <- typeTranslation p ty
            alloca <- instr (LLInstr.Alloca llty Nothing 0 []) (ptr llty)
            zoom symtab $ Env.insertValue str (Env.Value (Env.Pointer ty) alloca)
            let argRegister = LocalReference llty $ argName str
            voidInstr $ LLInstr.Store False alloca argRegister Nothing 0 []
        argName str = mkName $ str ++ "##arg"
