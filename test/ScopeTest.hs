module ScopeTest where

import Utility
import Scope

import Control.Monad ( liftM3 )
import Control.Monad.State
import Control.Monad.Except
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

instance Arbitrary SourcePos where
    arbitrary = liftM3 newPos arbitrary arbitrary arbitrary

instance Arbitrary a => Arbitrary (ScopeState a) where
    -- TODO: make this yield nested scopes.
    arbitrary = Scope.make <$> arbitrary

prop_lookupEmptyErrors :: String -> SourcePos -> Bool 
prop_lookupEmptyErrors s p =
    let res :: Except Error Bool
        res = evalStateT (Scope.lookup s p) Scope.empty
     in case runExcept res of Left _ -> True
                              _ -> False

