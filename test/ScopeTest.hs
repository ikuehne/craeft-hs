{-# LANGUAGE TemplateHaskell #-}

module ScopeTest where

import Craeft.Utility
import Craeft.Scope as Scope
import Control.Monad ( liftM3 )
import Control.Monad.State
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

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

prop_insertAvailableAfter :: String -> SourcePos -> Int -> ScopeState Int
                          -> Bool
prop_insertAvailableAfter name p x = withArbitraryState $ do
    insert name x
    new <- Scope.lookup name p
    return $ new == x

prop_lookupEmptyErrors :: String -> SourcePos -> Bool 
prop_lookupEmptyErrors s p = Scope.lookup s p `failsOn` empty

prop_nestingTemporary :: String -> SourcePos -> Int -> Int -> ScopeState Int
                      -> Bool
prop_nestingTemporary name pos old new = withArbitraryState $ do
    insert name old
    nested id $ insert name new
    current <- Scope.lookup name pos
    return $ current == old

return []
scopeTests = $quickCheckAll
