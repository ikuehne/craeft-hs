module Main where

import qualified Parser
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (evalStateT)
import Error
import qualified Environment
import qualified TypeChecker as TC
-- import qualified Codegen

main :: IO ()
main = do input <- getLine
          case runExcept (runParser input) of
               Left e -> prettyPrintError e
               Right r -> print r

runParser :: String -> CraeftExcept String
runParser s = do parseResult <- Parser.parseTopLevel "[stdin]" s
                 let checkedMonad = TC.typeCheckTopLevel parseResult
                 result <- evalStateT checkedMonad TC.initState
                 return (show $ contents result)
