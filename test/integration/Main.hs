{-# LANGUAGE CPP #-}

module Main where

import OperatorTest
import Framework
import System.Exit
import System.FilePath (splitFileName, pathSeparator)
import System.Process (readProcessWithExitCode)
import System.IO
import Test.Tasty

script = fst (splitFileName __FILE__) ++ pathSeparator : "run.py"

main = defaultMain $ testGroup "Integration Tests" [operatorTests]

{-
do (code, out, err) <- readProcessWithExitCode "python3" [script] ""
          putStr out
          hPutStr stderr err
          exitWith code
-}
