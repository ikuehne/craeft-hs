module Main where

import Text.Parsec
import qualified Parser

main :: IO ()
main = getLine >>= parseTest Parser.topLevel
