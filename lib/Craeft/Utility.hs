{-|
Module      : Craeft.Utility
Description : Error handling and related utilities.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Craeft.Utility ( Annotated (..)
                      , contents
                      , foldTraversals
                      , (!.)
                      , pos
                      , Error (..)
                      , CraeftExcept
                      , bracketed
                      , liftMaybe
                      , liftPos
                      , CraeftMonad
                      , MTraversal
                      , module Text.Parsec.Pos
                      , prettyPrintError
                      , renderError ) where

import Control.Lens
import Control.Exception (Exception)
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

instance Exception Error

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
liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe e = maybe (throwError e) return

prettyPrintError :: Error -> IO ()
prettyPrintError (ParseError msg p) = prettyPrintHelper "parse error" msg p
prettyPrintError (NameError msg p) = prettyPrintHelper "name error" msg p
prettyPrintError (TypeError msg p) = prettyPrintHelper "type error" msg p
prettyPrintError (InternalError msg) = printWithHeader "internal error" msg
prettyPrintError (UsageError msg) = printWithHeader "usage error" msg

renderError :: Error -> String
renderError (ParseError msg p) = renderHelper "parse error" msg p
renderError (NameError msg p) = renderHelper "name error" msg p
renderError (TypeError msg p) = renderHelper "type error" msg p
renderError (InternalError msg) = withHeader "internal error" msg
renderError (UsageError msg) = withHeader "usage error" msg

renderHelper header msg p = renderPos p ++ ": " ++ withHeader header msg
withHeader header msg = header ++ ": " ++ msg

instance Functor Annotated where
  fmap f (Annotated contents pos) = Annotated (f contents) pos

liftPos :: Monad m => Annotated (m a) -> m (Annotated a)
liftPos (Annotated m p) = flip Annotated p <$> m

headerColor :: IO ()
headerColor = hSetSGR stderr [SetColor Foreground Vivid Red]

resetColor :: IO ()
resetColor = hSetSGR stderr [Reset]

put :: String -> IO ()
put = hPutStr stderr

putln :: String -> IO ()
putln = hPutStrLn stderr


renderPos pos = sourceName pos ++ ":"
             ++ show (sourceLine pos) ++ ":"
             ++ show (sourceColumn pos)

prettyPrintHelper :: String -> String -> SourcePos -> IO ()
prettyPrintHelper header msg pos = do 
    put $ renderPos pos ++ ": "
    printWithHeader header msg

printWithHeader :: String -> String -> IO ()
printWithHeader header msg = do
    headerColor
    put header
    resetColor
    put ": "
    putln msg

-- | This almost certainly exists somewhere in some form already...
foldTraversals ts f c = foldr ((*>) . ($ c) . ($ f)) (pure c) ts

type MTraversal a b = forall m. Monad m => (b -> m b) -> a -> m a

-- | Data @Annotated@ with a source position.
data Annotated a = Annotated { _contents :: a, _pos :: SourcePos }
  deriving Show

makeLenses ''Annotated

-- | Access `l2` of the contents of `l1`.
(!.) l1 l2 = l1 . contents . l2
infixr 9 !.
