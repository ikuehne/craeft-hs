module Main where

import qualified Parser
import Control.Monad.Trans.Except
import Error
import qualified Environment

main :: IO ()
main = do result <- Parser.parseTopLevel "[stdin]" <$> getLine
          case runExcept result of
            Left e -> prettyPrintError e
            Right r -> print r
