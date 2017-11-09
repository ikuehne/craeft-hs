{-|
Module      : Craeft.TypedAST
Description : The typed Craeft Abstract Syntax Tree.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

Very similar to the @AST@, but with types represented as concrete Craeft types
rather than syntactic references.
-}

module Craeft.TypedAST ( module Craeft.TypedAST.Impl
                       , module Craeft.TypedAST.Lens ) where

import Craeft.TypedAST.Impl
import Craeft.TypedAST.Lens
