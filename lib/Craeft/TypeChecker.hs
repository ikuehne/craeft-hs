{-|
Module      : Craeft.TypeChecker
Description : The Craeft type-checking phase.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

Converts an untyped @AST@ to a typed @TAST@, and checks that types are
consistent in the process.
-}

{-# LANGUAGE TemplateHaskell #-}

module Craeft.TypeChecker ( typeCheck ) where

import           Control.Monad ( forM_, when )
import qualified Data.Map as Map
import qualified Data.Set as Set

import           Control.Lens
import           Control.Monad.Except (throwError)
import           Control.Monad.Trans.State (evalStateT)

import qualified Craeft.AST as AST
import qualified Craeft.TemplateFiller as Templ
import qualified Craeft.TypedAST as TAST
import           Craeft.Types
import           Craeft.Utility
import qualified Craeft.Scope as Scope

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
nested :: Checker a -> Checker a
nested = Scope.nested (zoom types) . Scope.nested (zoom variables)

-- | An initial map of signed types (I1-I1024).
signedTypes :: Map.Map String Type
signedTypes = Map.fromList [("I" ++ show i, Signed i) | i <- [1..1024]]

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
typeCheck :: AST.Program -> CraeftExcept TAST.Program
typeCheck program = evalStateT (mapM typeCheckTopLevel program) initState

-- | Convert an AST type to a Craeft type.
typeCheckType :: Annotated AST.Type -> Checker Type
typeCheckType (Annotated t p) = case t of
    AST.NamedType s targs ->
        do ty <- zoom types $ Scope.lookup s p
           case ty of
             Struct fields -> do
                 genedTargs <- mapM typeCheckType targs
                 filledTargs <- mapM (Templ.fillType p genedTargs . snd) fields
                 return $ Struct $ zip (map fst fields) filledTargs
             other -> return other
    AST.Void -> return Void
    AST.Pointer t' -> Pointer <$> typeCheckType t'

-- | Extract the type from an AST.ValueDeclaration.
declToType :: Annotated AST.ValueDeclaration -> Annotated AST.Type
declToType (Annotated (AST.ValueDeclaration ty _) p) = Annotated ty p

-- | Get the type of an annotated TAST expression.
exprType :: Lens' (Annotated TAST.Expression) Type
exprType = contents . TAST.exprType

-- | Run the type-checker on a function signature.
--
-- Enters the function into the scope, and returns the TAST signature.
typeCheckSig :: AST.FunctionSignature -> Checker TAST.FunctionSignature
typeCheckSig (AST.FunctionSignature name args targs retty') = do
    argtys <- mapM (typeCheckType . declToType) args
    retty <- typeCheckType retty'
    zoom variables $ Scope.globalInsert name (Function argtys retty)
    let typedArgs = zip (map (AST.name . view contents) args) argtys
        typedSig = TAST.Sig name typedArgs (length targs) retty
    return typedSig

-- | Type-check a top-level form.
typeCheckTopLevel :: Annotated AST.TopLevel
                  -> Checker (Annotated TAST.TopLevel)
typeCheckTopLevel (Annotated c p) = flip Annotated p <$> case c of
    AST.StructDeclaration name targs members -> do
        -- Allow recursive types.
        zoom types $ Scope.insert name (Opaque name)
        memberTypes <- nested $ do
            forM_ (zip [1..] targs) $ \(i, Annotated targ _) ->
                zoom types $ Scope.insert targ $ Hole i
            mapM (typeCheckType . declToType) members
        let names = map (AST.name . view contents) members
            memberAssocList = zip names memberTypes
        zoom types $ Scope.insert name $ Struct memberAssocList
        return $ TAST.StructDeclaration name memberAssocList (length targs)
    AST.TypeDeclaration name -> do
        zoom types $ Scope.insert name (Opaque name)
        return $ TAST.TypeDeclaration name
    AST.FunctionDecl sig -> do
        checkedSig <- typeCheckSig sig
        return $ TAST.FunctionDecl checkedSig
    AST.FunctionDefinition (Annotated sig _) body -> do
        checkedSig <- nested $ do
            forM_ (zip [1..] $ map (view contents) $ AST.functionTemplateArgs sig) $
                 \(i, name) -> zoom types $ Scope.insert name (Hole i)
            typeCheckSig sig
        
        nested $ do
            forM_ (checkedSig ^. TAST.args) $ \(name, ty) ->
                 zoom variables $ Scope.insert name ty
            forM_ (zip [1..] $ map (view contents) $ AST.functionTemplateArgs sig) $
                 \(i, name) -> zoom types $ Scope.insert name (Hole i)
            let checkStmt s = typeCheckStatement s (checkedSig ^. TAST.retty) 
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
        TAST.ExpressionStmt . view contents <$> typeCheckExpr e
    AST.Return e -> do
        -- Type-check the expression to be returned,
        checkedExpr <- typeCheckExpr e

        let foundTy = checkedExpr^.exprType

        -- and check that it matches the expected return type.
        when (foundTy /= retty) $
            throw p $ "returned value does not match expected return type "
                   ++ "\n\n(found: " ++ show foundTy
                   ++ "; expected: " ++ show retty ++ ")"
        return $ TAST.Return checkedExpr
    AST.VoidReturn -> do
        -- Check that we expected a void return.
        when (retty /= Void) $
            throw p "void return, but return type is not void"
        return TAST.VoidReturn
    AST.Assignment lhs rhs -> do
        -- Check the left-hand side
        (lhsTy, checkedLhs) <- typeCheckLValue p lhs
        -- and the right hand side.
        checkedRhs <- typeCheckExpr rhs
        -- Make sure they're compatible.
        when (lhsTy /= checkedRhs ^. exprType) $
            throw p "assigning to variable of different type"
        return $ TAST.Assignment checkedLhs checkedRhs
    AST.Declaration decl -> TAST.Declaration (AST.name decl) <$> checkDecl decl
    AST.CompoundDeclaration decl e -> do
        -- Get the declared type,
        expected <- checkDecl $ decl ^. contents
        -- and the type of the initializer expression.
        found <- typeCheckExpr e
        -- Check that they match.
        when (expected /= found ^. exprType) $
            throw p "compound assignment type mismatch"
        -- Extract the variable name from the declaration.
        let name = AST.name $ decl ^. contents
        return $ TAST.CompoundDeclaration name expected found
    AST.IfStatement cond ifb elseb -> do
        -- Type-check the condition.
        checkedCond <- typeCheckExpr cond
        -- Make sure it's a boolean.
        when (checkedCond ^. exprType /= Unsigned 1) $
            throw p "if condition must be U1"
        -- Check the `if` block (in a new scope).
        checkedIf <- nested $ mapM checkStmt ifb
        -- Check the `else` block (in a new scope).
        checkedElse <- nested $ mapM checkStmt elseb
        return $ TAST.IfStatement checkedCond checkedIf checkedElse
  where -- Type-check a statement in the current function.
        checkStmt = flip typeCheckStatement retty
        -- Type-check a variable declaration and add it to the scope.
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
        return (TAST.Reference $ lv ^. contents, Pointer ty)
    -- This is hard--pass it on to @inferBinopType@.
    AST.Binop lhs op rhs -> do
        checkedLhs <- typeCheckExpr lhs
        checkedRhs <- typeCheckExpr rhs
        ty <- inferBinopType p (checkedLhs ^. exprType)
                            op (checkedRhs ^. exprType)
        return (TAST.Binop checkedLhs op checkedRhs, ty)
    -- Use the return type of the function.
    AST.FunctionCall f args targs -> do
        checkedF <- typeCheckExpr f
        checkedArgs <- mapM typeCheckExpr args
        checkedTargs <- mapM typeCheckType targs
        case checkedF ^. exprType of
            Function args ret -> do
                args <- mapM (Templ.fillType p checkedTargs) args
                ret <- Templ.fillType p checkedTargs ret

                when (map (view exprType) checkedArgs /= args) $
                    throw p "argument types do not match function signature"
                return ( TAST.FunctionCall checkedF checkedArgs checkedTargs
                       , ret)
            _ -> throw p "cannot call non-function"
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
        return (TAST.LValueExpr $ checkedLv ^. contents, ty)

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

errNoLogicalOps = flip throw $ "cannot perform logical operations between types"
                            ++ " other than U1"

-- | Infer the result type of a binary operator expression.
--
-- Takes as arguments the operator (as a string) and the types of the LHS and
-- RHS operands.
inferBinopType :: SourcePos -> Type -> String -> Type -> Checker Type
inferBinopType p (Signed l) s (Signed r) 
  | s `Set.member` comparisonOps = return $ Unsigned 1
  | s `Set.member` logicalOps = errNoLogicalOps p
  | s `Set.member` Set.union bitwiseOps arithmeticOps =
      return $ Signed $ max l r
  | otherwise = throwError $
        InternalError $ "type checker received unrecognized op" ++ s
-- Signed/unsigned operations coerced to signed/signed operations.
inferBinopType p lhs@(Signed _) s (Unsigned r) =
    inferBinopType p lhs s (Signed r)
inferBinopType p (Unsigned l) s rhs@(Signed _) =
    inferBinopType p (Signed l) s rhs
inferBinopType p (Unsigned l) s (Unsigned r)
  | s `Set.member` comparisonOps = return $ Unsigned 1
  | s `Set.member` logicalOps =
      throw p "cannot perform logical operation between integers"
  | s `Set.member` Set.union bitwiseOps arithmeticOps =
      return $ Unsigned $ max l r
  | otherwise = throwError $
        InternalError $ "type checker received unrecognized op" ++ s
inferBinopType p (Pointer t1) s (Pointer t2) 
  | t1 /= t2 = throw p "cannot compare pointers of different types"
  | s `Set.member` comparisonOps = return $ Unsigned 1
  | s == "-" = return $ Signed 64
  | otherwise = throw p "cannot do arithmetic between pointers"
inferBinopType p (Floating precl) s (Floating precr)
  | s `Set.member` comparisonOps = return $ Unsigned 1
  | s `Set.member` logicalOps = errNoLogicalOps p
  | s `Set.member` bitwiseOps = throw p $ "can't perform bitwise operation on "
                                       ++ "floating-point values"
  | s `Set.member` arithmeticOps = return $ Floating (max precl precr)
inferBinopType p (Floating _) _ _ =
    throw p "cannot perform operation between floating-point and another type"
inferBinopType p _ _ (Floating _) =
    throw p "cannot perform operation between floating-point and another type"
inferBinopType _ (Signed _) "+" (Pointer t) = return $ Pointer t
inferBinopType _ (Unsigned _) "+" (Pointer t) = return $ Pointer t
inferBinopType _ (Pointer t) "+" (Signed _) = return $ Pointer t
inferBinopType _ (Pointer t) "+" (Unsigned _) = return $ Pointer t
inferBinopType _ (Pointer t) "-" (Signed _) = return $ Pointer t
inferBinopType _ (Pointer t) "-" (Unsigned _) = return $ Pointer t
inferBinopType p _ _ _ = throw p
    "type checker doesn't know how to deal with these yet"

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
            _ -> throw p "cannot dereference non-pointer type"
        let expr = TAST.Expression contents t
        return (t, TAST.Dereference (Annotated expr p'))
    AST.FieldAccess e n -> do
        str <- typeCheckExpr (Annotated e p)
        case str ^. exprType of 
            Struct fields -> do
                (ty, i) <- liftMaybe (TypeError "no such field in struct" p)
                                     (lookupI n fields)
                let strExpr = str ^. contents
                return (ty, TAST.FieldAccess strExpr i)
            o -> throw p $ "cannot access field of non-struct value of type"
                        ++ show o
  where lookupI v l = lookup v [(k, (v, i)) | ((k, v), i) <- zip l [0..]]

throw :: SourcePos -> String -> Checker a
throw p e = throwError $ TypeError e p
