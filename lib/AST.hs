{-|
Module      : AST
Description : The untyped Craeft Abstract Syntax Tree.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

The Craeft abstract syntax tree consists of a series of transparent algebraic
data types, containing the information needed to run codegen and the typechecker
as well as source-position annotations for error messages (see @Utility@).
-}

module AST where

import qualified Text.Parsec.Pos as Pos

import Utility

-- | Function signatures: everything needed to call a function.
data FunctionSignature = FunctionSignature {
    fnName :: String
  , args :: [Annotated ValueDeclaration]
  , retty :: Annotated Type
} deriving Show

type Program = [Annotated TopLevel]

-- | Top-level forms: forms that can appear outside of any other form.
data TopLevel =
    StructDeclaration { structName :: String
                      , structMembers :: [Annotated ValueDeclaration] }
  | TypeDeclaration String
  | FunctionDecl FunctionSignature
  | FunctionDefinition { sig :: Annotated FunctionSignature
                       , body :: Block }
  deriving Show

--
-- Statements.
--

-- | Variable declarations.
data ValueDeclaration = ValueDeclaration { ty :: Type
                                         , name :: String } deriving Show

type Block = [Annotated Statement]

data Statement =
    ExpressionStatement (Annotated Expression)
  | Return (Annotated Expression)
  | VoidReturn
  | Assignment { leftOfEquals :: LValue
               , rightOfEquals :: Annotated Expression }
  -- ^ A declaration consists of a type and a name.
  | Declaration ValueDeclaration
  -- ^ A compound declaration is a type, a name, and an initial value for the
  -- new variable.
  | CompoundDeclaration { declaredVariable :: Annotated ValueDeclaration
                        , initialValue :: Annotated Expression }
  | IfStatement { cond :: Annotated Expression
                , ifBlock :: Block
                , elseBlock :: Block }
  deriving Show

-- | The AST representation of a type--the way the type is named in Craeft.
data Type =
    NamedType String
  | Void
  | Pointer (Annotated Type)
  deriving Show

-- | Craeft expressions.
--
-- This AST includes no direct information on the types of expressions; see
-- @TAST@ for one that does.
data Expression =
    IntLiteral Integer
  -- ^ Unsigned (i.e. non-negative) integer literals.
  | UIntLiteral Integer
  | FloatLiteral Double
  | StringLiteral String
  -- ^ An application of the address-of (&) operator.
  | Reference LValue
  -- ^ An application of a binary operator.
  | Binop { lhs :: Annotated Expression
          , op :: String
          , rhs :: Annotated Expression }
  | FunctionCall { func :: Annotated Expression,
                   callArgs :: [Annotated Expression] }
  -- ^ A type cast.
  | Cast { toType :: Annotated Type
         , value :: Annotated Expression }
  | LValueExpr LValue
  deriving Show

data LValue =
    Variable String
  | Dereference Expression
  | FieldAccess { struct :: Expression
                , fieldName :: String }
  deriving Show
