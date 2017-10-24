module Main where

import Control.Monad (when)
import System.Exit

import ScopeTest
import Test.QuickCheck

main :: IO ()
main = do success <- scopeTests
          when (not success) $ exitWith $ ExitFailure 1
