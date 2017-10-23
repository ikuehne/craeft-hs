{-|
Module      : Craeft.Scope
Description : An abstract mapping type, allowing nested maps.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental

Used to represent Craeft scopes.
-}

{-# LANGUAGE TemplateHaskell #-}

module Craeft.Scope ( -- * Basic data types.
                      Scope 
                    , ScopeState
                      -- * Creating new scopes.
                    , Craeft.Scope.empty
                    , make
                      -- * Nesting scopes.
                    , nested
                      -- * Map operations.
                    , Craeft.Scope.lookup
                    , insert ) where

import           Control.Applicative
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State

import           Craeft.Utility

newtype ScopeState a = ScopeState { _scopes :: [Map String a] }
makeLenses ''ScopeState

type Scope a r = CraeftMonad (ScopeState a) r

-- | Create a new scope starting with the given map.
make :: Map String a -> ScopeState a
make m = ScopeState [m]

-- | An empty scope.
empty :: ScopeState a
empty = make Map.empty

-- | Execute the given action in a fresh scope using the given scope accessor.
--
-- Exception-safe: if the action throws, the scope will still be popped.  We
-- expose this as the only way to nest scopes because we want to ensure that
-- every @push@ has a corresponding @pop@.
nested :: (Scope a () -> CraeftMonad s ()) -> CraeftMonad s r -> CraeftMonad s r
nested scope action = scope push >> bracketed (scope pop) action

-- | Look up the given name in the scope, starting at the innermost scope.
lookup :: String -> SourcePos -> Scope a a
lookup k p = do ss <- use scopes
                let results = Map.lookup k <$> ss
                liftMaybe (NameError msg p) $ foldr (<|>) Nothing results
  where msg = "name not found in scope: " ++ k

-- | Insert a new value in the innermost scope.
insert :: String -> a -> Scope a ()
insert k v = whenNonEmpty $ \s ss -> put (ScopeState $ Map.insert k v s : ss)

--
-- Lower-level operations.
--

push :: Scope a ()
push = scopes %= (Map.empty :)

pop :: Scope a ()
pop = whenNonEmpty $ const $ put . ScopeState

-- | Do an operation on the head and tail of a non-empty @Scope@.
whenNonEmpty :: (Map String a -> [Map String a] -> Scope a r) -> Scope a r
whenNonEmpty f = use scopes >>= liftMaybe emptyPop . List.uncons >>= uncurry f
  where emptyPop = InternalError "attempt to pop empty environment"
