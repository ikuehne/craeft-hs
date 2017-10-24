module ScopeTest where

import Craeft.Utility
import Craeft.Scope as Scope
import Control.Monad ( liftM3 )
import Control.Monad.State
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Utility

instance Arbitrary SourcePos where
    arbitrary = liftM3 newPos arbitrary arbitrary arbitrary

instance Arbitrary a => Arbitrary (ScopeState a) where
    -- Yes, this is disgusting, and could be improved by expanding the interface
    -- of @Scope@.  However, that would have to mean removing some of its safety
    -- guarantees (namely, that a scope cannot be pushed without a corresponding
    -- pop).
    arbitrary = liftM2 (!!) nestings (abs <$> arbitrary)
      where nestings = flip iterate empty
                     . withArbitraryState
                     . addNesting
                     <$> arbitrary
            addNesting :: [(String, a)] -> Scope a (ScopeState a)
            addNesting randoms = nested id $ do
                mapM_ (uncurry insert) randoms
                get

insertAvailableAfter :: String -> SourcePos -> Int -> ScopeState Int -> Bool
insertAvailableAfter name p x = withArbitraryState $ do
    insert name x
    new <- Scope.lookup name p
    return $ new == x

lookupEmptyErrors :: String -> SourcePos -> Bool 
lookupEmptyErrors s p = Scope.lookup s p `failsOn` empty

nestingTemporary :: String -> SourcePos -> Int -> Int -> ScopeState Int -> Bool
nestingTemporary name pos old new = withArbitraryState $ do
    insert name old
    nested id $ insert name new
    current <- Scope.lookup name pos
    return $ current == old

scopeTests = testGroup "Scope Tests" [
    QC.testProperty "name available after insert" insertAvailableAfter
  , QC.testProperty "lookup on empty throws" lookupEmptyErrors
  , QC.testProperty "nested scopes are temporary" nestingTemporary ]
