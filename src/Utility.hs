{-|
Module      : Utility
Description : Error handling and related utilities.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

module Utility ( Annotated (..)
               , Error (..)
               , CraeftExcept
               , throwC
               , CraeftMonad
               , SourcePos
               , prettyPrintError ) where

import System.Console.ANSI
import System.IO
import qualified Text.Parsec.Pos as Pos
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (StateT)

--
-- Pure, monadic error handling.
--

-- | The possible types of errors.
--
-- Every type of error contains a @String@ message, which should *not* be
-- capitalized or contain any kind of header or position information; a colored
-- header and clang-style pointer into the source can be added with
-- @prettyPrintError@ below.
data Error = ParseError String SourcePos
           | NameError String SourcePos
           | TypeError String SourcePos
           -- ^ These are errors that the user should never see.  Try to avoid
           -- using them...
           | InternalError String
           | UsageError String
  deriving Show

type SourcePos = Pos.SourcePos

-- | An exception monad with @Error@ as the error type.
type CraeftExcept = Except Error

-- | A state/exception monad transformer stack with @Error@ as the error type.
type CraeftMonad s a = StateT s CraeftExcept a

throwC :: Error -> CraeftMonad s a
throwC = lift . throwE 

prettyPrintError :: Error -> IO ()
prettyPrintError (ParseError msg p) = prettyPrintHelper "parse error" msg p
prettyPrintError (NameError msg p) = prettyPrintHelper "name error" msg p
prettyPrintError (TypeError msg p) = prettyPrintHelper "type error" msg p
prettyPrintError (InternalError msg) = printWithHeader "internal error" msg
prettyPrintError (UsageError msg) = printWithHeader "usage error" msg

-- | Data @Annotated@ with a source position.
data Annotated a = Annotated { contents :: a, pos :: SourcePos }
  deriving Show



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
    put $ Pos.sourceName pos ++ ":" 
    put $ show (Pos.sourceLine pos) ++ ":"
    put $ show (Pos.sourceColumn pos) ++ ": "
    printWithHeader header msg

printWithHeader :: String -> String -> IO ()
printWithHeader header msg = do
    headerColor
    put header
    resetColor
    put ": "
    putln msg
