{-|
Module      : Craeft.Utility
Description : Error handling and related utilities.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

module Craeft.Utility ( Annotated (..)
                      , Error (..)
                      , CraeftExcept
                      , bracketed
                      , liftMaybe
                      , CraeftMonad
                      , module Text.Parsec.Pos
                      , prettyPrintError ) where

import Data.Maybe (maybe)
import System.Console.ANSI
import System.IO

import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (StateT)
import Text.Parsec.Pos

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

-- | An exception monad with @Error@ as the error type.
type CraeftExcept = Except Error

-- | A state/exception monad transformer stack with @Error@ as the error type.
type CraeftMonad s a = StateT s CraeftExcept a

-- | Safely run a cleanup action after another action.
--
-- Ensures that cleanup is run regardless of whether the other action succeeds
-- or fails.
bracketed :: MonadError e m => m () -> m a -> m a
bracketed cleanup action = do
    ret <- action `catchError` (\e -> cleanup >> throwError e)
    cleanup
    return ret

-- | Bring a @Maybe@ into a @CraeftMonad@
--
-- Throw the given error on @Nothing@.
liftMaybe :: Error -> Maybe a -> CraeftMonad s a
liftMaybe e = maybe (throwError e) return

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
    put $ sourceName pos ++ ":" 
    put $ show (sourceLine pos) ++ ":"
    put $ show (sourceColumn pos) ++ ": "
    printWithHeader header msg

printWithHeader :: String -> String -> IO ()
printWithHeader header msg = do
    headerColor
    put header
    resetColor
    put ": "
    putln msg
