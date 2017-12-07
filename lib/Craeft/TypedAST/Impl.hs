{-|
Module      : Craeft.TypedAST.Impl
Description : The typed Craeft Abstract Syntax Tree.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

Contains the concrete types for the AST.  Must be separated from @TypedAST.Lens@
because Template Haskell has problems with mutually recursive structures.
-}

module Craeft.TypedAST.Impl where

import Craeft.Types
import Craeft.Utility

type Program = [Annotated TopLevel]

data FunctionSignature = Sig { _name :: String
                             , _args :: [(String, Type)]
                             , _fntargs :: Int
                             , _retty :: Type }
  deriving Show

data TopLevel = Function { _sig :: FunctionSignature
                         , _body :: Block }
              | FunctionDecl { _declSig :: FunctionSignature }
              | TypeDeclaration { _declTypeName :: String }
              | StructDeclaration { _structName :: String
                                  , _members :: [(String, Type)]
                                  , _sntargs :: Int }
  deriving Show

type Block = [Annotated Statement]

data Statement = ExpressionStmt { _stmtExpr :: Expression }
               | Return { _retExpr :: Annotated Expression }
               | VoidReturn
               | Assignment { _leftOfEquals :: Annotated LValue
                            , _rightOfEquals :: Annotated Expression }
               | Declaration { _varName :: String
                             , _varType :: Type }
               | CompoundDeclaration { _compVarName :: String
                                     , _compVarType :: Type
                                     , _compInit :: Annotated Expression }
               | IfStatement { _cond :: Annotated Expression
                             , _ifBlock :: Block
                             , _elseBlock :: Block }
  deriving Show

data ExpressionContents = IntLiteral { _signedLit :: Integer }
                        | UIntLiteral { _unsignedLit :: Integer }
                        | FloatLiteral { _floatLit :: Double }
                        | StringLiteral { _stringLit :: String }
                        | Reference { _referencedVal :: LValue }
                        | Binop { _lhs :: Annotated Expression
                                , _op :: String
                                , _rhs :: Annotated Expression }
                        | FunctionCall { _func :: Annotated Expression
                                       , _callArgs :: [Annotated Expression]
                                       , typeArgs :: [Type] }
                        | Cast { _castedExpr :: Annotated Expression }
                        | LValueExpr { _lvalueExpr :: LValue }
  deriving Show

data Expression = Expression { _exprContents :: ExpressionContents
                             , _exprType :: Type }
  deriving Show

data LValue = Variable { _lvalueVarName :: String}
            | Dereference { _derefPointer :: Annotated Expression }
            | FieldAccess { _structExpr :: Expression
                          , _fieldIdx :: Integer }
  deriving Show
