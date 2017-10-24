module Utility where

import Data.Either

import Control.Monad.Except
import Control.Monad.State
import Craeft.Utility

succeedsOn :: CraeftMonad s a -> s ->  Bool
succeedsOn action state = isRight $ runExcept $ evalStateT action state

failsOn :: CraeftMonad s a -> s -> Bool
failsOn = (not .) . succeedsOn

withArbitraryState :: CraeftMonad s a -> s -> a
withArbitraryState action state =
    case runExcept $ evalStateT action state of
         Left _ -> error "attempted to extract from error"
         Right a -> a
