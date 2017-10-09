{-# LANGUAGE TemplateHaskell #-}

module Scope ( Scope 
             , ScopeState
             , Scope.empty
             , make
             , push
             , pop
             , Scope.lookup
             , insert ) where

import Control.Applicative
import qualified Data.Map as Map

import Control.Lens
import Control.Monad.Except
import Control.Monad.State

import Error

newtype ScopeState a = ScopeState { _scopes :: [Map.Map String a] }
makeLenses ''ScopeState

type Scope a r = CraeftMonad (ScopeState a) r

make :: Map.Map String a -> ScopeState a
make m = ScopeState [m]

empty :: ScopeState a
empty = make Map.empty

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
