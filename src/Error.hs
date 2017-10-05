module Error ( Annotated (..)
             , Error (..)
             , CraeftExcept
             , prettyPrintError ) where

import System.Console.ANSI
import System.IO
import Text.Parsec.Pos
import Control.Monad.Trans.Except

data Annotated a = Annotated { contents :: a, pos :: SourcePos }
  deriving Show

data Error = ParseError String SourcePos
           | NameError String SourcePos
           | InternalError String
  deriving Show

type CraeftExcept = Except Error

prettyPrintError :: Error -> IO ()
prettyPrintError (ParseError msg p) = prettyPrintHelper "parse error" msg p
prettyPrintError (NameError msg p) = prettyPrintHelper "name error" msg p
prettyPrintError (InternalError msg) = printWithHeader "internal error" msg

headerColor :: IO ()
headerColor = hSetSGR stderr [SetColor Foreground Vivid Red]

resetColor :: IO ()
resetColor = hSetSGR stderr [Reset]

put :: String -> IO ()
put = hPutStr stderr

putln :: String -> IO ()
putln = hPutStrLn stderr

prettyPrintHelper :: String -> String -> SourcePos -> IO ()
prettyPrintHelper header msg pos = do 
    put $ sourceName pos ++ ":" 
    put $ show (sourceLine pos) ++ ":" ++ show (sourceColumn pos) ++ ": "
    printWithHeader header msg

printWithHeader :: String -> String -> IO ()
printWithHeader header msg = do
    headerColor
    put header
    resetColor
    put ": "
    putln msg
