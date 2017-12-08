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
          | Unsigned Int
          | Signed Int
          | Floating Precision
          | Function [Type] Type Int
          | Opaque
          | Hole Int
          | Void
  deriving (Eq, Ord, Show)

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

holePointer :: Type -> Bool
holePointer (Pointer (Hole _)) = True
holePointer _ = False
