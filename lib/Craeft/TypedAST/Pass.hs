{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
{-# LANGUAGE LambdaCase #-}

module Craeft.TypedAST.Pass where

import Control.Lens
import Control.Monad
import Debug.Trace (trace)

import           Craeft.TypedAST.Impl
import           Craeft.TypedAST.Lens
import qualified Craeft.Types as Types
import           Craeft.Types (Type)
import           Craeft.Utility

-- | A traversal into all leaf expressions.
eachLeafExpression :: Traversal' ExpressionContents ExpressionContents
eachLeafExpression f expr = case expr of
    Reference _ ->
        (referencedVal.eachLValueExpr.exprContents.eachLeafExpression) f expr
    Binop lhs op rhs -> flip Binop op <$> subExprs f lhs <*> subExprs f rhs
    FunctionCall func args targs -> FunctionCall
                                <$> subExprs f func
                                <*> (each.subExprs) f args
                                <*> pure targs
    Cast _ -> (castedExpr.subExprs $ f) expr
    LValueExpr _ ->
        (lvalueExpr.eachLValueExpr.exprContents.eachLeafExpression) f expr
    other -> f other
  where subExprs = contents.exprContents.eachLeafExpression

eachExprFunctionCall :: Traversal' ExpressionContents (Annotated Expression,
                                                       [Annotated Expression],
                                                       [Types.Type] )
eachExprFunctionCall f expr = case expr of
    Reference _ ->
        (referencedVal.eachLValueExpr.exprContents.eachExprFunctionCall) f expr
    Binop lhs op rhs -> flip Binop op <$> subExprs f lhs <*> subExprs f rhs
    FunctionCall func args targs -> uncurry3 FunctionCall
                                <$> f (func, args, targs)
    Cast _ -> (castedExpr.subExprs $ f) expr
    LValueExpr _ ->
        (lvalueExpr.eachLValueExpr.exprContents.eachExprFunctionCall) f expr
    other -> pure other
  where subExprs = contents.exprContents.eachExprFunctionCall
        uncurry3 f (x, y, z) = f x y z

eachLValueExpr :: Traversal' LValue Expression
eachLValueExpr f d@(Dereference _) = (derefPointer.contents) f d
eachLValueExpr f (FieldAccess e i) =
    FieldAccess <$> f e <*> pure i
eachLValueExpr f other = pure other

-- | Does *not* include sub-statements.
--
-- To do that, just do `eachSubStmt.eachExpressionInStmt`.
eachExpressionInStmt :: Traversal' Statement Expression
eachExpressionInStmt f stmt = case stmt of
    ExpressionStmt e -> stmtExpr f stmt
    Return ret -> (retExpr.contents) f stmt
    Assignment l r -> Assignment <$> (contents.eachLValueExpr) f l
                                 <*> contents f r
    CompoundDeclaration n t e -> CompoundDeclaration n t
                             <$> contents f e
    IfStatement c ifB elseB -> IfStatement
                           <$> contents f c
                           <*> (each.contents.eachExpressionInStmt) f ifB
                           <*> (each.contents.eachExpressionInStmt) f elseB
    other -> pure other

eachSubStmt :: Traversal' Statement Statement
eachSubStmt f stmt = case stmt of
    IfStatement c ifB elseB -> IfStatement c
                           <$> (each.contents.eachSubStmt) f ifB
                           <*> (each.contents.eachSubStmt) f elseB
    other -> f other

eachStatementInBlock :: Traversal' Block Statement
eachStatementInBlock = each.contents.eachSubStmt
