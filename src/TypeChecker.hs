{-# LANGUAGE TemplateHaskell #-}

module TypeChecker where

import Control.Arrow ((***))
import Control.Monad ( sequence, when )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Control.Lens

import qualified AST
import qualified TypedAST as TAST
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

inNewScope :: Checker a -> Checker a
inNewScope c = do zoom types Scope.push
                  zoom variables Scope.push
                  res <- c
                  zoom types Scope.pop
                  zoom variables Scope.pop
                  return res

signedTypes :: Map.Map String Type
signedTypes = let fromInt i = ("I" ++ show i, Signed i)
               in Map.fromList $ map fromInt [1..1024]

unsignedTypes :: Map.Map String Type
unsignedTypes = let fromInt i = ("U" ++ show i, Unsigned i)
                 in Map.fromList $ map fromInt [1..1024]

floatTypes :: Map.Map String Type
floatTypes = Map.fromList [ ("Float", Floating SinglePrec)
                          , ("Double", Floating DoublePrec)]

initialTypes :: Scope.ScopeState Type
initialTypes = Scope.make $ Map.unions [signedTypes, unsignedTypes, floatTypes]

initState = CheckerState initialTypes Scope.empty

typeCheck :: AST.Program -> Checker TAST.Program
typeCheck = mapM typeCheckTopLevel

typeCheckType :: Annotated AST.Type -> Checker Type
typeCheckType (Annotated t p) = case t of
    AST.NamedType s -> zoom types $ Scope.lookup s p
    AST.Void -> return Void
    AST.Pointer t' -> Pointer <$> typeCheckType t'

declToType (Annotated (AST.ValueDeclaration ty _) p) = Annotated ty p

typeCheckSig :: AST.FunctionSignature -> Checker TAST.FunctionSignature
typeCheckSig (AST.FunctionSignature name args retty') = do
    argtys <- mapM (typeCheckType . declToType) args
    retty <- typeCheckType retty'
    zoom variables $ Scope.insert name (Function argtys retty)
    let typedArgs = zip (map (AST.name . contents) args) argtys
        typedSig = TAST.Sig name typedArgs retty
    return typedSig

typeCheckTopLevel :: Annotated AST.TopLevel
                  -> Checker (Annotated TAST.TopLevel)
typeCheckTopLevel (Annotated c p) = flip Annotated p <$> case c of
    AST.StructDeclaration name members -> do
        -- Allow recursive types.
        zoom types $ Scope.insert name Opaque
        memberTypes <- mapM (typeCheckType . declToType) members
        let names = map (AST.name . contents) members
            memberAssocList = zip names memberTypes
        zoom types $ Scope.insert name $ Struct memberAssocList
        return $ TAST.StructDeclaration name memberAssocList
    AST.TypeDeclaration name -> do
        zoom types $ Scope.insert name Opaque
        return $ TAST.TypeDeclaration name
    AST.FunctionDecl sig -> do
        checkedSig <- typeCheckSig sig
        return $ TAST.FunctionDecl checkedSig
    AST.FunctionDefinition (Annotated sig _) body -> do
        checkedSig <- typeCheckSig sig
        inNewScope $ do
            sequence_ [zoom variables $ Scope.insert name ty
                          | (name, ty) <- TAST.args checkedSig]
            let checkStmt s = typeCheckStatement s (TAST.retty checkedSig) 
            checkedBody <- mapM checkStmt body
            return $ TAST.Function checkedSig checkedBody

typeCheckStatement :: Annotated AST.Statement
                   -> Type
                   -> Checker (Annotated TAST.Statement)
typeCheckStatement (Annotated s p) retty = flip Annotated p <$> case s of
    AST.ExpressionStatement e ->
        TAST.ExpressionStmt . contents <$> typeCheckExpr e
    AST.Return e -> do
        checkedExpr <- typeCheckExpr e
        when (getType checkedExpr /= retty) badReturn
        return $ TAST.Return checkedExpr
    AST.VoidReturn -> do
        when (retty /= Void) badReturn
        return TAST.VoidReturn
    AST.Assignment lhs rhs -> do
        (lhsTy, checkedLhs) <- typeCheckLValue p lhs
        checkedRhs <- typeCheckExpr rhs
        when (lhsTy /= getType checkedRhs) $
            throw "assigning to variable of different type"
        return $ TAST.Assignment checkedLhs checkedRhs
    AST.Declaration decl ->
        TAST.Declaration (AST.name decl) <$> checkDecl decl
    AST.CompoundDeclaration decl e -> do
        expected <- checkDecl $ contents decl
        found <- typeCheckExpr e
        when (expected /= getType found) $
            throw "compound assignment type mismatch"
        let name = AST.name $ contents decl
        return $ TAST.CompoundDeclaration name expected found
    AST.IfStatement cond ifb elseb -> do
        checkedCond <- typeCheckExpr cond
        when (getType checkedCond /= Unsigned 1) $
            throw "if condition must be U1"
        checkedIf <- inNewScope $ mapM checkStmt ifb
        checkedElse <- inNewScope $ mapM checkStmt elseb
        return $ TAST.IfStatement checkedCond checkedIf checkedElse
  where badReturnMsg = "return type does not match function signature"
        badReturn = throwC $ TypeError badReturnMsg p
        checkStmt = flip typeCheckStatement retty
        getType = TAST.exprType . contents
        throw = throwC . flip TypeError p
        checkDecl (AST.ValueDeclaration ty' name) = do
            ty <- typeCheckType (Annotated ty' p)
            zoom variables $ Scope.insert name ty
            return ty

typeCheckExpr :: Annotated AST.Expression
              -> Checker (Annotated TAST.Expression)
typeCheckExpr (Annotated expr p) =
        (\(e, t) -> Annotated (TAST.Expression e t) p) <$> case expr of
    AST.IntLiteral i -> return (TAST.IntLiteral i, Signed 64)
    AST.UIntLiteral u -> return (TAST.UIntLiteral u, Unsigned 64)
    AST.FloatLiteral f -> return (TAST.FloatLiteral f, Floating DoublePrec)
    AST.StringLiteral s -> return (TAST.StringLiteral s, Pointer (Unsigned 8))
    AST.Reference lvalue -> do 
        (ty, lv) <- typeCheckLValue p lvalue
        return (TAST.Reference $ contents lv, Pointer ty)
    AST.Binop lhs op rhs -> do
        checkedLhs <- typeCheckExpr lhs
        checkedRhs <- typeCheckExpr rhs
        ty <- inferBinopType p (exprTy checkedLhs) op (exprTy checkedRhs)
        return (TAST.Binop checkedLhs op checkedRhs, ty)
    AST.FunctionCall f args -> do
        checkedF <- typeCheckExpr f
        checkedArgs <- mapM typeCheckExpr args
        case exprTy checkedF of
            Function args ret -> do
                when (map exprTy checkedArgs /= args) $
                    throw "argument types do not match function signature"
                return (TAST.FunctionCall checkedF checkedArgs, ret)
            _ -> throw "cannot call non-function"
    AST.Cast to from -> do
        checkedFrom <- typeCheckExpr from
        toTy <- typeCheckType to
        return (TAST.Cast checkedFrom, toTy)
    AST.LValueExpr lv -> do
        (ty, checkedLv) <- typeCheckLValue p lv
        return (TAST.LValueExpr $ contents checkedLv, ty)
  where exprTy = TAST.exprType . contents
        throw = throwC . flip TypeError p

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
                -> AST.LValue
                -> Checker (Type, Annotated TAST.LValue)
typeCheckLValue p lv = (\(t, l) -> (t, Annotated l p)) <$> case lv of
    AST.Variable s -> do
        ty <- zoom variables $ Scope.lookup s p
        return (ty, TAST.Variable s)
    AST.Dereference e -> do
        expr <- typeCheckExpr (Annotated e p)
        return (exprTy expr, TAST.Dereference expr)
    AST.FieldAccess e n -> do
        str <- typeCheckExpr (Annotated e p)
        case exprTy str of 
            Struct fields -> case lookupI n fields of
                Nothing -> throw "no such field in struct"
                Just (ty, i) -> return (ty, TAST.FieldAccess (contents str) i)
            _ -> throw "cannot access field of non-struct value"
  where exprTy = TAST.exprType . contents
        throw msg = throwC $ TypeError msg p
        lookupI v l = lookup v [(k, (v, i)) | ((k, v), i) <- zip l [0..]]
