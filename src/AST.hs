module AST ( Annotated (..)
           , FunctionSignature (..)
           , TopLevel (..)
           , ValueDeclaration (..)
           , Block
           , Statement (..)
           , Type (..)
           , Expression (..)
           , LValue (..)
) where

import qualified Text.Parsec.Pos as Pos

--
-- General.
--

data Annotated a = Annotated { contents :: a, pos :: Pos.SourcePos }
  deriving Show
--
-- Top-level syntax.
--

data FunctionSignature = FunctionSignature {
    fnName :: String
  , args :: [Annotated ValueDeclaration]
  , retty :: Annotated Type
} deriving Show

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
  | Assignment { leftOfEquals :: Annotated LValue
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
  | Reference (Annotated LValue)
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
  | Dereference (Annotated Expression)
  | FieldAccess { struct :: Annotated Expression
                , fieldName :: String }
  deriving Show
