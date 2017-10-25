{-|
Module      : Craeft.Types
Description : Internal representation of Craeft types.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

module Craeft.Types where

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

integral :: Type -> Bool
integral (Signed _) = True
integral (Unsigned _) = True
integral _ = False

floating :: Type -> Bool
floating (Floating _) = True
floating _ = False

pointer :: Type -> Bool
pointer (Pointer _) = True
pointer _ = False
