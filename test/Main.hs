module Main where

import ScopeTest
import Test.QuickCheck

main = quickCheck prop_lookupEmptyErrors
