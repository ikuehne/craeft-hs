{-|
Module      : AST
Description : The untyped Craeft Abstract Syntax Tree.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

The Craeft abstract syntax tree consists of a series of transparent algebraic
data types, containing the information needed to run codegen and the typechecker
as well as source-position annotations for error messages (see @Error@).
-}

module AST where

import qualified Text.Parsec.Pos as Pos

import Error
import qualified Environment as Env

--
-- Top-level syntax.
--

data FunctionSignature = FunctionSignature {
    fnName :: String
  , args :: [Annotated ValueDeclaration]
  , retty :: Annotated Type
} deriving Show

type Program = [Annotated TopLevel]

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

data ValueDeclaration = ValueDeclaration { ty :: Type
                                         , name :: String } deriving Show

type Block = [Annotated Statement]

data Statement =
    ExpressionStatement (Annotated Expression)
  | Return (Annotated Expression)
  | VoidReturn
  | Assignment { leftOfEquals :: LValue
               , rightOfEquals :: Annotated Expression }
  | Declaration ValueDeclaration
  | CompoundDeclaration { declaredVariable :: Annotated ValueDeclaration
                        , initialValue :: Annotated Expression }
  | IfStatement { cond :: Annotated Expression
                , ifBlock :: Block
                , elseBlock :: Block }
  deriving Show

--
-- Types.
--

data Type =
    NamedType String
  | Void
  | Pointer (Annotated Type)
  deriving Show

--
-- Expressions.
--

data Expression =
    IntLiteral Integer
  | UIntLiteral Integer
  | FloatLiteral Double
  | StringLiteral String
  | Reference LValue
  | Binop { lhs :: Annotated Expression
          , op :: String
          , rhs :: Annotated Expression }
  | FunctionCall { func :: Annotated Expression,
                   callArgs :: [Annotated Expression] }
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
