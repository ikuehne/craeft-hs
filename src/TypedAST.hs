module TypedAST where

import Environment ( Type )
import Error

type Program = [Annotated TopLevel]

data FunctionSignature = Sig { name :: String
                             , args :: [(String, Type)]
                             , retty :: Type }
  deriving Show

data TopLevel = Function { sig :: FunctionSignature
                         , body :: Block }
              | FunctionDecl FunctionSignature
              | TypeDeclaration String
              | StructDeclaration { structName :: String
                                  , members :: [(String, Type)] }
  deriving Show

type Block = [Annotated Statement]

data Statement = ExpressionStmt Expression
               | Return (Annotated Expression)
               | VoidReturn
               | Assignment { leftOfEquals :: Annotated LValue
                            , rightOfEquals :: Annotated Expression }
               | Declaration String Type
               | CompoundDeclaration String Type (Annotated Expression)
               | IfStatement { cond :: Annotated Expression
                             , ifBlock :: Block
                             , elseBlock :: Block }
  deriving Show

data ExpressionContents = IntLiteral Integer
                        | UIntLiteral Integer
                        | FloatLiteral Double
                        | StringLiteral String
                        | Reference LValue
                        | Binop { lhs :: Annotated Expression
                                , op :: String
                                , rhs :: Annotated Expression }
                        | FunctionCall { func :: Annotated Expression,
                                         callArgs :: [Annotated Expression] }
                        | Cast (Annotated Expression)
                        | LValueExpr LValue
  deriving Show

data Expression = Expression { exprContents :: ExpressionContents
                             , exprType :: Type }
  deriving Show

data LValue = Variable String
            | Dereference (Annotated Expression)
            | FieldAccess { struct :: Expression
                          , fieldIdx :: Integer }
  deriving Show
