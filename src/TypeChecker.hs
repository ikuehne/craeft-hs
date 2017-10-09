{-# LANGUAGE TemplateHaskell #-}

module TypeChecker ( typeCheck ) where

import Control.Arrow ((***))
import Control.Monad ( sequence, when )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Control.Lens

import qualified AST
import Environment ( Type (..)
                   , Precision (..) )
import Error
import qualified Scope

-- `_types` contains a mapping from type names to types, e.g. I64 to 64-bit
-- signed integer.  `variables` contains a mapping from variables to their
-- types.
data CheckerState = CheckerState { _types :: Scope.ScopeState Type
                                 , _variables :: Scope.ScopeState Type }
makeLenses ''CheckerState

type Checker a = CraeftMonad CheckerState a

push :: Checker ()
push = zoom types Scope.push >> zoom variables Scope.push

pop :: Checker ()
pop = zoom types Scope.pop >> zoom variables Scope.pop

signedTypes :: Map.Map String Type
signedTypes = let fromInt i = ("I" ++ show i, Signed i)
               in Map.fromList $ fromInt <$> [0..1024]

unsignedTypes :: Map.Map String Type
unsignedTypes = let fromInt i = ("U" ++ show i, Unsigned i)
                 in Map.fromList $ fromInt <$> [0..1024]

floatTypes :: Map.Map String Type
floatTypes = Map.fromList [ ("Float", Floating SinglePrec)
                          , ("Double", Floating DoublePrec)]

initialTypes :: Scope.ScopeState Type
initialTypes = Scope.make $ Map.unions [signedTypes, unsignedTypes, floatTypes]

initState = CheckerState initialTypes Scope.empty

typeCheck :: AST.Program -> Checker AST.TypedProgram
typeCheck = mapM typeCheckTopLevel

typeCheckType :: Annotated AST.Type -> Checker Type
typeCheckType (Annotated t p) = case t of
    AST.NamedType s -> zoom types $ Scope.lookup s p
    AST.Void -> return Void
    AST.Pointer t' -> Pointer <$> typeCheckType t'

declToType (Annotated (AST.ValueDeclaration ty _) p) = Annotated ty p

typeCheckSig :: AST.FunctionSignature -> Checker Type
typeCheckSig (AST.FunctionSignature name args retty) = do
    argtys <- mapM (typeCheckType . declToType) args
    retty <- typeCheckType retty
    zoom variables $ Scope.insert name (Function argtys retty)
    return retty

typeCheckTopLevel :: Annotated AST.PositionedTopLevel
                  -> Checker (Annotated AST.TypedTopLevel)
typeCheckTopLevel (Annotated c p) = flip Annotated p <$> case c of
    AST.StructDeclaration name members -> do
        memberTypes <- mapM (typeCheckType . declToType) members
        let names = map (AST.name . AST.contents) members
        zoom types $ Scope.insert name (Struct $ zip names memberTypes)
        return $ AST.StructDeclaration name members
    AST.TypeDeclaration name -> do
        zoom types $ Scope.insert name Opaque
        return $ AST.TypeDeclaration name
    AST.FunctionDecl sig -> do
        typeCheckSig sig
        return $ AST.FunctionDecl sig
    AST.FunctionDefinition annotatedSig@(Annotated sig _) body -> do
        retty <- typeCheckSig sig
        push
        checkedBody <- mapM (`typeCheckStatement` retty) body
        pop
        return $ AST.FunctionDefinition annotatedSig checkedBody

typeCheckStatement :: Annotated AST.PositionedStatement
                   -> Type
                   -> Checker (Annotated AST.TypedStatement)
typeCheckStatement (Annotated s p) retty = flip Annotated p <$> case s of
    AST.ExpressionStatement e -> AST.ExpressionStatement <$> typeCheckExpr e
    AST.Return e -> do
        ty <- getType <$> typeCheckExpr e
        when (ty /= retty) $
            throwC badReturn
        AST.Return <$> typeCheckExpr e
    AST.VoidReturn -> do
        when (retty /= Void) $
            throwC badReturn
        return AST.VoidReturn
    AST.Assignment lhs rhs -> do
        (lhsTy, checkedLhs) <- typeCheckLValue p lhs
        checkedRhs <- typeCheckExpr rhs
        when (lhsTy /= getType checkedRhs) $
            throwC $ TypeError "assigning to variable of different type" p
        return $ AST.Assignment checkedLhs checkedRhs
    AST.Declaration decl -> checkDecl decl >> return (AST.Declaration decl)
    AST.CompoundDeclaration decl e -> do
        expected <- checkDecl $ contents decl
        found <- typeCheckExpr e
        when (expected /= getType found) $
            throwC $ TypeError "compound assignment type mismatch" p
        return $ AST.CompoundDeclaration decl found
    AST.IfStatement cond ifb elseb -> do
        checkedCond <- typeCheckExpr cond
        let condTy = getType checkedCond
        when (condTy /= Unsigned 1) $
            throwC $ TypeError "if condition must be U1" p
        push
        checkedIf <- mapM checkStmt ifb
        pop
        push
        checkedElse <- mapM checkStmt elseb
        pop
        return $ AST.IfStatement checkedCond checkedIf checkedElse
  where badReturnMsg = "return type does not match function signature"
        badReturn = TypeError badReturnMsg p
        getType = snd . AST.annotation
        checkStmt = flip typeCheckStatement retty
        checkDecl (AST.ValueDeclaration ty' name) = do
            ty <- typeCheckType (Annotated ty' p)
            zoom variables $ Scope.insert name ty
            return ty

typeCheckExpr :: AST.PositionedExpression -> Checker AST.TypedExpression
typeCheckExpr (AST.EW expr p) = (\(e, t) -> AST.EW e (p, t)) <$> case expr of
    AST.IntLiteral i -> return (AST.IntLiteral i, Signed 64)
    AST.UIntLiteral u -> return (AST.UIntLiteral u, Unsigned 64)
    AST.FloatLiteral f -> return (AST.FloatLiteral f, Floating DoublePrec)
    AST.StringLiteral s -> return (AST.StringLiteral s, Pointer (Unsigned 8))
    AST.Reference lvalue -> do (ty, lv) <- typeCheckLValue p lvalue
                               return (AST.Reference lv, Pointer ty)
    AST.Binop lhs op rhs -> do
        checkedLhs <- typeCheckExpr lhs
        checkedRhs <- typeCheckExpr rhs
        ty <- inferBinopType p (exprTy checkedLhs) op (exprTy checkedRhs)
        return (AST.Binop checkedLhs op checkedRhs, ty)
    AST.FunctionCall f args -> do
        checkedF <- typeCheckExpr f
        checkedArgs <- mapM typeCheckExpr args
        case exprTy checkedF of
            Function args ret -> do
                when (map exprTy checkedArgs /= args) $
                    throw "argument types do not match function signature"
                return (AST.FunctionCall checkedF checkedArgs, ret)
            _ -> throw "cannot call non-function"
    AST.Cast to from -> do
        checkedFrom <- typeCheckExpr from
        toTy <- typeCheckType to
        return (AST.Cast to checkedFrom, toTy)
    AST.LValueExpr lv -> do
        (ty, checkedLv) <- typeCheckLValue p lv
        return (AST.LValueExpr checkedLv, ty)
  where exprTy = snd . AST.annotation
        throw msg = throwC $ TypeError msg p

comparisonOps = Set.fromList ["==", "!=", ">=", "<=", ">", "<"]
logicalOps = Set.fromList ["&&", "||"]
bitwiseOps = Set.fromList ["&", "|"]
arithmeticOps = Set.fromList ["+", "*", "/", "-"]

inferBinopType :: SourcePos -> Type -> String -> Type -> Checker Type
inferBinopType p (Signed l) s (Signed r) 
  | s `Set.member` comparisonOps = return $ Unsigned 1
  | s `Set.member` logicalOps =
      throwC $ TypeError "cannot perform logical operation between integers" p
  | s `Set.member` Set.union bitwiseOps arithmeticOps =
      return $ Signed $ max l r
  | otherwise =
        throwC $ InternalError $ "type checker received unrecognized op" ++ s
-- Signed/unsigned operations coerced to signed/signed operations.
inferBinopType p lhs@(Signed _) s (Unsigned r) =
    inferBinopType p lhs s (Signed r)
inferBinopType p (Unsigned l) s rhs@(Signed _) =
    inferBinopType p (Signed l) s rhs
inferBinopType p (Unsigned l) s (Unsigned r)
  | s `Set.member` comparisonOps = return $ Unsigned 1
  | s `Set.member` logicalOps =
      throwC $ TypeError "cannot perform logical operation between integers" p
  | s `Set.member` Set.union bitwiseOps arithmeticOps =
      return $ Unsigned $ max l r
  | otherwise =
        throwC $ InternalError $ "type checker received unrecognized op" ++ s
inferBinopType p _ s _ = throwC $
    TypeError "type checker doesn't know how to deal with these yet" p

typeCheckLValue :: SourcePos
                -> AST.PositionedLValue
                -> Checker (Type, AST.TypedLValue)
typeCheckLValue p lv = case lv of
    AST.Variable s -> do
        expr <- zoom variables $ Scope.lookup s p
        return (expr, AST.Variable s)
    AST.Dereference e -> do
        expr <- typeCheckExpr e
        return (exprTy expr, AST.Dereference expr)
    AST.FieldAccess e n -> do
        str <- typeCheckExpr e
        case exprTy str of 
            Struct fields -> case List.lookup n fields of
                Nothing -> throw "no such field in struct"
                Just ty -> return (ty, AST.FieldAccess str n)
            _ -> throw "cannot access field of non-struct value"
  where exprTy = snd . AST.annotation
        throw msg = throwC $ TypeError msg p
