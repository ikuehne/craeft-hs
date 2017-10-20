{-|
Module      : Scope
Description : An abstract mapping type, allowing pushing and popping.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

Designed to implement Craeft scopes.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Scope ( -- * Basic data types.
               Scope 
             , ScopeState
               -- * Creating new scopes.
             , Scope.empty
             , make
               -- * Nesting scopes.
             , nested
             , push
             , pop
               -- * Map operations.
             , Scope.lookup
             , insert ) where

import Control.Applicative
import qualified Data.Map as Map

import Control.Lens
import Control.Monad.Except
import Control.Monad.State

import Utility

newtype ScopeState a = ScopeState { _scopes :: [Map.Map String a] }
makeLenses ''ScopeState

type Scope a r = CraeftMonad (ScopeState a) r

make :: Map.Map String a -> ScopeState a
make m = ScopeState [m]

empty :: ScopeState a
empty = make Map.empty

-- | Execute the given action using the given scope lens.
nested :: forall a b s. Lens' s (ScopeState b)
       -> CraeftMonad s a
       -> CraeftMonad s a
nested scope action = do zoom scope push
                         result <- action `catchError` \e -> do
                             zoom scope pop
                             throwC e
                         zoom scope pop
                         return result

pushMap :: Map.Map String a -> Scope a ()
pushMap m = scopes %= (m :)

push :: Scope a ()
push = pushMap Map.empty

emptyPop = throwC $ InternalError "attempt to pop empty environment"

pop :: Scope a ()
pop = do ss <- use scopes
         case ss of [] -> emptyPop
                    s:ss -> put (ScopeState ss)

lookup :: String -> SourcePos -> Scope a a
lookup k p = do ss <- use scopes
                let results = Map.lookup k <$> ss
                case foldr (<|>) Nothing results of
                                    Nothing -> throwC $ NameError msg p
                                    Just x  -> return x
  where msg = "name not found in scope: " ++ k

insert :: String -> a -> Scope a ()
insert k v = do ss <- use scopes
                case ss of [] -> emptyPop
                           s:ss -> put (ScopeState $ Map.insert k v s : ss)
