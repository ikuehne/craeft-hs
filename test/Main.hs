module Main where

import Control.Monad (when)
import System.Exit

import Test.Tasty

import ScopeTest
import TypeCheckerTest
import TemplTest
import OperatorTest

main :: IO ()
main = defaultMain $ testGroup "Tests" [
      testGroup "Unit Tests" [ scopeTests
                             , typeCheckerTests ]
    , testGroup "Integration Tests" [ operatorTests
                                    , templTests ] ]
