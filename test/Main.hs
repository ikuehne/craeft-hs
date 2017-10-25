module Main where

import Control.Monad (when)
import System.Exit

import ScopeTest
import TypeCheckerTest
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Unit Tests" [
    scopeTests
  , typeCheckerTests]
