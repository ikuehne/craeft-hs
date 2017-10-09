module AST ( Annotated (..)
           , FunctionSignature (..)
           , TopLevel (..)
           , ValueDeclaration (..)
           , Block
           , Statement (..)
           , Type (..)
           , Expression (..)
           , LValue (..) 
           , ExpressionWith (..)
           , PositionedExpression
           , PositionedLValue
           , TypedLValue
           , PositionedStatement
           , TypedStatement
           , PositionedTopLevel
           , TypedProgram
           , TypedTopLevel
           , Program
           , TypedExpression ) where

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

type Program = [Annotated (TopLevel PositionedExpression)]
type TypedProgram = [Annotated TypedTopLevel]

data TopLevel e =
    StructDeclaration { structName :: String
                      , structMembers :: [Annotated ValueDeclaration] }
  | TypeDeclaration String
  | FunctionDecl FunctionSignature
  | FunctionDefinition { sig :: Annotated FunctionSignature
                       , body :: Block e }
  deriving Show

type PositionedTopLevel = TopLevel PositionedExpression
type TypedTopLevel = TopLevel TypedExpression

--
-- Statements.
--

data ValueDeclaration = ValueDeclaration { ty :: Type
                                         , name :: String } deriving Show

type Block e = [Annotated (Statement e)]

data Statement e =
    ExpressionStatement e
  | Return e
  | VoidReturn
  | Assignment { leftOfEquals :: LValue e
               , rightOfEquals :: e }
  | Declaration ValueDeclaration
  | CompoundDeclaration { declaredVariable :: Annotated ValueDeclaration
                        , initialValue :: e }
  | IfStatement { cond :: e
                , ifBlock :: Block e
                , elseBlock :: Block e }
  deriving Show

type PositionedStatement = Statement PositionedExpression
type TypedStatement = Statement TypedExpression

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

data Expression e =
    IntLiteral Integer
  | UIntLiteral Integer
  | FloatLiteral Double
  | StringLiteral String
  | Reference (LValue e)
  | Binop { lhs :: e
          , op :: String
          , rhs :: e }
  | FunctionCall { func :: e,
                   callArgs :: [e] }
  | Cast { toType :: Annotated Type
         , value :: e }
  | LValueExpr (LValue e)
  deriving Show

data LValue e =
    Variable String
  | Dereference e
  | FieldAccess { struct :: e
                , fieldName :: String }
  deriving Show

data ExpressionWith a = EW { ewContents :: Expression (ExpressionWith a)
                           , annotation :: a }
  deriving Show

type PositionedExpression = ExpressionWith Pos.SourcePos
type TypedExpression = ExpressionWith (Pos.SourcePos, Env.Type)
type PositionedLValue = LValue PositionedExpression
type TypedLValue = LValue TypedExpression
