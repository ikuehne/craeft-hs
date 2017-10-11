{-|
Module      : TypeChecker
Description : The Craeft type-checking phase.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

Converts an untyped @AST@ to a typed @TAST@, and checks that types are
consistent in the process.
-}

{-# LANGUAGE TemplateHaskell #-}

module TypeChecker ( Checker
                   , CheckerState
                   , typeCheckTopLevel
                   , initState ) where

import Control.Monad ( sequence_
                     , when )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Control.Lens

import qualified AST
import qualified TypedAST as TAST
import Environment ( Type (..)
                   , Precision (..) )
import Utility
import qualified Scope

-- | The state for the type checker.
data CheckerState = CheckerState { -- ^ A mapping from type names to types;
                                   -- e.g. "I64" to 64-bit signed integer.
                                   _types :: Scope.ScopeState Type
                                   -- ^ A mapping from variables to their
                                   -- types.
                                 , _variables :: Scope.ScopeState Type }
makeLenses ''CheckerState

-- | The type-checker monad.
--
-- Allows exceptions and modifying the @CheckerState@, but is completely pure.
type Checker a = CraeftMonad CheckerState a

--
-- Utilities for dealing with scopes.
--

-- | Run the given Checker monad in a fresh scope.
--
-- I.e., push a scope before running it, and pop the scope after running it.
inNewScope :: Checker a -> Checker a
inNewScope c = do zoom types Scope.push
                  zoom variables Scope.push
                  res <- c
                  zoom types Scope.pop
                  zoom variables Scope.pop
                  return res

-- | An initial map of signed types (I1-I1024).
signedTypes :: Map.Map String Type
signedTypes = let fromInt i = ("I" ++ show i, Signed i)
               in Map.fromList $ map fromInt [1..1024]

-- | An initial map of unsigned types (U1-U1024).
unsignedTypes :: Map.Map String Type
unsignedTypes = let fromInt i = ("U" ++ show i, Unsigned i)
                 in Map.fromList $ map fromInt [1..1024]

-- | An initial map of floats (Double, Float).
floatTypes :: Map.Map String Type
floatTypes = Map.fromList [ ("Float", Floating SinglePrec)
                          , ("Double", Floating DoublePrec)]

-- | The combined map of built-in types.
initialTypes :: Scope.ScopeState Type
initialTypes = Scope.make $ Map.unions [signedTypes, unsignedTypes, floatTypes]

-- | The initial state of the type-checker.
initState :: CheckerState
initState = CheckerState initialTypes Scope.empty

-- | Type-check a program.
typeCheck :: AST.Program -> Checker TAST.Program
typeCheck = mapM typeCheckTopLevel

-- | Convert an AST type to a Craeft type.
typeCheckType :: Annotated AST.Type -> Checker Type
typeCheckType (Annotated t p) = case t of
    AST.NamedType s -> zoom types $ Scope.lookup s p
    AST.Void -> return Void
    AST.Pointer t' -> Pointer <$> typeCheckType t'

-- | Extract the type from an AST.ValueDeclaration.
declToType :: Annotated AST.ValueDeclaration -> Annotated AST.Type
declToType (Annotated (AST.ValueDeclaration ty _) p) = Annotated ty p

-- | Get the type of an annotated TAST expression.
exprToType :: Annotated TAST.Expression -> Type
exprToType = TAST.exprType . contents

-- | Run the type-checker on a function signature.
--
-- Enters the function into the scope, and returns the TAST signature.
typeCheckSig :: AST.FunctionSignature -> Checker TAST.FunctionSignature
typeCheckSig (AST.FunctionSignature name args retty') = do
    argtys <- mapM (typeCheckType . declToType) args
    retty <- typeCheckType retty'
    zoom variables $ Scope.insert name (Function argtys retty)
    let typedArgs = zip (map (AST.name . contents) args) argtys
        typedSig = TAST.Sig name typedArgs retty
    return typedSig

-- | Type-check a top-level form.
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

-- | Type-check an AST statement.
typeCheckStatement :: Annotated AST.Statement
                   -- ^ The return type of the containing function.
                   -> Type
                   -> Checker (Annotated TAST.Statement)
typeCheckStatement (Annotated s p) retty = flip Annotated p <$> case s of
    AST.ExpressionStatement e ->
        -- Simple: just unwrap and type-check the expression.
        TAST.ExpressionStmt . contents <$> typeCheckExpr e
    AST.Return e -> do
        -- Type-check the expression to be returned,
        checkedExpr <- typeCheckExpr e
        -- and check that it matches the expected return type.
        when (exprToType checkedExpr /= retty) $
            throw "returned value does not match expected return type"
        return $ TAST.Return checkedExpr
    AST.VoidReturn -> do
        -- Check that we expected a void return.
        when (retty /= Void) $
            throw "void return, but return type is not void"
        return TAST.VoidReturn
    AST.Assignment lhs rhs -> do
        -- Check the left-hand side
        (lhsTy, checkedLhs) <- typeCheckLValue p lhs
        -- and the right hand side.
        checkedRhs <- typeCheckExpr rhs
        -- Make sure they're compatible.
        when (lhsTy /= exprToType checkedRhs) $
            throw "assigning to variable of different type"
        return $ TAST.Assignment checkedLhs checkedRhs
    AST.Declaration decl ->
        TAST.Declaration (AST.name decl) <$> checkDecl decl
    AST.CompoundDeclaration decl e -> do
        -- Get the declared type,
        expected <- checkDecl $ contents decl
        -- and the type of the initializer expression.
        found <- typeCheckExpr e
        -- Check that they match.
        when (expected /= exprToType found) $
            throw "compound assignment type mismatch"
        -- Extract the variable name from the declaration.
        let name = AST.name $ contents decl
        return $ TAST.CompoundDeclaration name expected found
    AST.IfStatement cond ifb elseb -> do
        -- Type-check the condition.
        checkedCond <- typeCheckExpr cond
        -- Make sure it's a boolean.
        when (exprToType checkedCond /= Unsigned 1) $
            throw "if condition must be U1"
        -- Check the `if` block (in a new scope).
        checkedIf <- inNewScope $ mapM checkStmt ifb
        -- Check the `else` block (in a new scope).
        checkedElse <- inNewScope $ mapM checkStmt elseb
        return $ TAST.IfStatement checkedCond checkedIf checkedElse
  where checkStmt = flip typeCheckStatement retty
        throw = throwC . flip TypeError p
        checkDecl (AST.ValueDeclaration ty' name) = do
            ty <- typeCheckType (Annotated ty' p)
            zoom variables $ Scope.insert name ty
            return ty

-- | Type-check an expression.
typeCheckExpr :: Annotated AST.Expression
              -> Checker (Annotated TAST.Expression)
typeCheckExpr (Annotated expr p) =
        -- Do some bookkeeping--the inner case expression returns a (TAST
        -- expression, type) tuple which we want to convert to an annotated TAST
        -- expression.
        (\(e, t) -> Annotated (TAST.Expression e t) p) <$> case expr of
    -- Literals are all simple--just use the default type.
    AST.IntLiteral i -> return (TAST.IntLiteral i, Signed 64)
    AST.UIntLiteral u -> return (TAST.UIntLiteral u, Unsigned 64)
    AST.FloatLiteral f -> return (TAST.FloatLiteral f, Floating DoublePrec)
    AST.StringLiteral s -> return (TAST.StringLiteral s, Pointer (Unsigned 8))
    -- Return a pointer to the inner type.
    AST.Reference lvalue -> do 
        (ty, lv) <- typeCheckLValue p lvalue
        return (TAST.Reference $ contents lv, Pointer ty)
    -- This is hard--pass it on to @inferBinopType@.
    AST.Binop lhs op rhs -> do
        checkedLhs <- typeCheckExpr lhs
        checkedRhs <- typeCheckExpr rhs
        ty <- inferBinopType p (exprToType checkedLhs)
                            op (exprToType checkedRhs)
        return (TAST.Binop checkedLhs op checkedRhs, ty)
    -- Use the return type of the function.
    AST.FunctionCall f args -> do
        checkedF <- typeCheckExpr f
        checkedArgs <- mapM typeCheckExpr args
        case exprToType checkedF of
            Function args ret -> do
                when (map exprToType checkedArgs /= args) $
                    throw "argument types do not match function signature"
                return (TAST.FunctionCall checkedF checkedArgs, ret)
            _ -> throw "cannot call non-function"
    -- Use the result type of the cast.
    AST.Cast toType fromExpr -> do
        checkedFrom <- typeCheckExpr fromExpr
        checkedTo <- typeCheckType toType
        return (TAST.Cast checkedFrom, checkedTo)
    -- LValues just get passed on to @typeCheckLValue@.  Note that during
    -- codegen, we always treat l-values as pointers, so that we can assign to
    -- them using a load; the type checker *does not do that*.
    AST.LValueExpr lv -> do
        (ty, checkedLv) <- typeCheckLValue p lv
        return (TAST.LValueExpr $ contents checkedLv, ty)
  where -- Throw a type error at our position with the given message.
        throw = throwC . flip TypeError p

-- Some useful divisions of the operators.  Each of these sets has the same way
-- of calculating the result types.

-- So comparison operators work on any types, and return a @U1@,
comparisonOps = Set.fromList ["==", "!=", ">=", "<=", ">", "<"]
-- logical operators work on @U1@s and return @U1@s,
logicalOps = Set.fromList ["&&", "||"]
-- bitwise operators work on integers and return integers,
bitwiseOps = Set.fromList ["&", "|"]
-- and arithmetic operators work on integers, floating point numbers, and
-- sometimes pointers.
arithmeticOps = Set.fromList ["+", "*", "/", "-"]

-- | Infer the result type of a binary operator expression.
--
-- Takes as arguments the operator (as a string) and the types of the LHS and
-- RHS operands.
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

-- | Type-check an l-value.
--
-- Note that this returns the actual type of the l-value, *not* a pointer type
-- to it.
typeCheckLValue :: SourcePos
                -> AST.LValue
                -> Checker (Type, Annotated TAST.LValue)
typeCheckLValue p lv = (\(t, l) -> (t, Annotated l p)) <$> case lv of
    AST.Variable s -> do
        ty <- zoom variables $ Scope.lookup s p
        return (ty, TAST.Variable s)
    AST.Dereference e -> do
        Annotated (TAST.Expression contents ty) p' <-
            typeCheckExpr (Annotated e p)
        t <- case ty of
            Pointer t -> return t
            _ -> throw "cannot dereference non-pointer type"
        return (ty, TAST.Dereference (Annotated contents p') t)
    AST.FieldAccess e n -> do
        str <- typeCheckExpr (Annotated e p)
        case exprToType str of 
            Struct fields -> case lookupI n fields of
                Nothing -> throw "no such field in struct"
                Just (ty, i) ->
                    let strExpr = TAST.exprContents $ contents str
                     in return (ty, TAST.FieldAccess strExpr fields i)
            _ -> throw "cannot access field of non-struct value"
  where throw msg = throwC $ TypeError msg p
        lookupI v l = lookup v [(k, (v, i)) | ((k, v), i) <- zip l [0..]]
