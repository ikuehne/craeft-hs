module Utility where

import Data.Either

import Control.Monad.Except
import Control.Monad.State
import Craeft.Utility

import Test.Tasty.QuickCheck as QC

instance Arbitrary SourcePos where
    arbitrary = liftM3 newPos arbitrary arbitrary arbitrary

instance Arbitrary a => Arbitrary (Annotated a) where
    arbitrary = liftM2 Annotated arbitrary arbitrary

annotate = flip Annotated (newPos "filename" 0 0)

succeedsOn :: CraeftMonad s a -> s ->  Bool
succeedsOn action state = isRight $ runExcept $ evalStateT action state

failsOn :: CraeftMonad s a -> s -> Bool
failsOn = (not .) . succeedsOn

withArbitraryState :: CraeftMonad s a -> s -> a
withArbitraryState action state =
    case runExcept $ evalStateT action state of
         Left _ -> error "attempted to extract from error"
         Right a -> a
