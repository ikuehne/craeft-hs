{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
{-# LANGUAGE QuasiQuotes #-}

module TemplTest ( templTests ) where

import Debug.Trace (trace)
import Data.Int
import Data.Word

import Craeft.Parser as Parser
import Craeft.Utility
import Craeft.TypeChecker as TC
import Data.String.Interpolate
import Test.Tasty
import Test.Tasty.HUnit
import System.Random

import Integration

templTests = testGroup "template tests" $ map (uncurry makeSizeofTest) [
    ("U64", 8),
    ("I64", 8),
    ("U64 *", 8),
    ("I64 *", 8),
    ("Float", 4),
    ("Double", 8)
  ]

cHarness = [i|

#include <stdio.h>
#include <stdint.h>

int64_t size(void);

int main(int argc, char **argv) {
    printf("%d", size());
    return 0;
}

|]

-- | Build a Craeft program which exports a single operator as a symbol.
craeftSizeof ty = [i|

fn<:T:> sizeof() -> I64 {
    T *x;
    return (U8 *)(x + 1) - (U8 *)x;
}

fn size() -> I64 {
    return sizeof<: #{ty} :>();
}

|]

-- | Make an arithmetic test for a given type.
makeSizeofTest tyName tySize =
    testCase [i|sizeof<:#{tyName}:> works|] $
        deterministicProperty (craeftSizeof tyName) cHarness [] $
                        assertEqual msg tySize . read
  where msg = [i|sizeof<:#{tyName}:>|]
