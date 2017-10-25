{-# LANGUAGE CPP #-}

module Main where

import System.Exit
import System.FilePath (splitFileName, pathSeparator)
import System.Process (readProcessWithExitCode)
import System.IO

script = fst (splitFileName __FILE__) ++ pathSeparator : "run.py"

main = do (code, out, err) <- readProcessWithExitCode "python3" [script] ""
          putStr out
          hPutStr stderr err
          exitWith code
