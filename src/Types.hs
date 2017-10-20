{-|
Module      : Types
Description : Internal representation of Craeft types.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

module Types ( Type (..)
             , Precision (..) ) where

data Precision = SinglePrec | DoublePrec
  deriving (Eq, Ord, Show)

data Type = Struct [(String, Type)]
          | Pointer Type
          | Signed Int
          | Unsigned Int
          | Function [Type] Type
          | Floating Precision
          | Opaque
          | Void
  deriving (Eq, Show)
