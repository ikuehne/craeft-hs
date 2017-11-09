{-|
Module      : Craeft.TypedASTImpl
Description : Lenses for the typed Craeft AST.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

{-# LANGUAGE TemplateHaskell #-}

module Craeft.TypedAST.Lens where

import Control.Lens
import Craeft.TypedAST.Impl

makeLenses ''FunctionSignature
makeLenses ''TopLevel
makeLenses ''ExpressionContents
makeLenses ''Expression
makeLenses ''LValue
