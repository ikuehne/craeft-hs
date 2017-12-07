{-# LANGUAGE FlexibleContexts #-}

module Craeft.TemplateFiller where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Writer
import qualified Data.Map as Map

import           Craeft.TypedAST
import qualified Craeft.TypedAST.Pass as Pass
import           Craeft.Types as Types
import           Craeft.Utility

--
-- Traversing over the types in an AST.
--

-- | Traverse over the types in an expression.
eachExprType :: Traversal' Expression Types.Type
eachExprType f (Expression contents ty) =
  Expression <$> eachExprContentsType f contents <*> f ty

-- | Traverse over all of the types an an `ExpressionContents`.
eachExprContentsType :: Traversal' ExpressionContents Types.Type
eachExprContentsType f expr = case expr of
    Reference _ -> (referencedVal.Pass.eachLValueExpr.eachExprType $ f) expr
    Binop lhs op rhs -> flip Binop op <$> subExprTys f lhs <*> subExprTys f rhs
    FunctionCall func args targs -> FunctionCall
                                <$> subExprTys f func
                                <*> (each.subExprTys) f args
                                -- TODO: add this invocation to the worklist.
                                <*> pure targs
    Cast _ -> (castedExpr.subExprTys $ f) expr
    LValueExpr _ -> (lvalueExpr.eachLValueExpr.eachExprType $ f) expr
    other -> pure other
  where subExprTys = contents.eachExprType

-- | Traverse over all types in each statement.
eachStatementType :: MTraversal Statement Type
eachStatementType f = (eachExpressionInStmt.eachExprType) f
                  -- Visit the types in variable declarations.
                  >=> varType f >=> compVarType f

--
-- The state that we need to keep track of while we fill templates.
--

type FunctionSpecialization = ([Types.Type], Expression)

type TemplatizerWorklist = WriterT [FunctionSpecialization] CraeftExcept

data TemplatizeInstance = TemplInst { templateArgs :: [Types.Type]
                                    , instancePos :: SourcePos }

type Templatize a = ReaderT TemplatizeInstance TemplatizerWorklist a

type Templatizer a = a -> Templatize a

fillFunction :: [Types.Type] -> SourcePos
             -> (FunctionSignature, Block)
             -> CraeftExcept ([FunctionSpecialization], FunctionSignature, Block)
fillFunction ts p func =
    do ((sig, block), others) <-
          runWriterT $ runTemplatizer templatizeFunction ts p func
       return (others, sig, block)

runTemplatizer :: Templatizer a -> [Types.Type] -> SourcePos -> a
               -> TemplatizerWorklist a
runTemplatizer t args p x = writer
  where inst = TemplInst args p
        reader = t x
        writer = runReaderT reader inst

templatizeStatement :: Templatizer Statement
templatizeStatement = eachStatementType %%~ fillTypeTempl

templatizeBlock :: Templatizer Block
templatizeBlock = each.contents %%~ templatizeStatement

templatizeSig :: Templatizer FunctionSignature
templatizeSig = args.each._2 %%~ fillTypeTempl
            >=> retty %%~ fillTypeTempl
            >=> fntargs %%~ const (pure 0)

templatizeFunction :: Templatizer (FunctionSignature, Block)
templatizeFunction (sig, block) =
    do block <- templatizeBlock block
       sig <- templatizeSig sig
       return (sig, block)

--
-- Handy utilities for the Templatize monad.
--

fillTypeTempl :: Type -> Templatize Type
fillTypeTempl t = do p <- asks instancePos 
                     ts <- asks templateArgs
                     fillType p ts t

-- | Fill any holes in the given type.
fillType :: MonadError Error m => SourcePos -> [Type] -> Type -> m Type
fillType p ts (Hole i) = let maybeHole = Map.lookup i holesMap
                          in liftMaybe (noSuchHole i p) maybeHole
  where holesMap = Map.fromList $ zip [1..] ts
        noSuchHole i = TypeError ("no such hole: " ++ show i)
fillType p ts (Struct fields) = Struct <$> (each._2 %%~ fillType p ts) fields
fillType p ts (Pointer other) = Pointer <$> fillType p ts other
fillType _ _ other = return other
