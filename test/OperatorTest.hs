{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification, Rank2Types #-}
{-# LANGUAGE QuasiQuotes #-}

module OperatorTest ( operatorTests ) where

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

operatorTests = testGroup "operator tests" $ makeOperatorTestSuite <$> [
      ArithmeticOp (+) (+) "+" "addition"
    , ArithmeticOp (-) (-) "-" "subtraction"
    , ArithmeticOp quot (/) "/" "division"
    , ArithmeticOp (*) (*) "*" "multiplication" ]

--
-- Building C harnesses.
--

-- | Build a simple C harness.
--
-- Declares a 2-argument function @craeftfn@, of type
-- @ltype@ -> @rtype@ -> @rettype@.  Then reads in two command-line arguments
-- using format strings @lfmt@ and @rfmt@, and prints out the result of
-- @craeftfn@ on them using @retfmt@.
simpleCHarness ltype rtype rettype lfmt rfmt retfmt = [i|

#include <stdio.h>
#include <stdint.h>
#{rettype} craeftfn(#{ltype}, #{rtype});
int main(int argc, char **argv) {
    #{ltype} a;
    #{rtype} b;
    sscanf(argv[1], "#{lfmt}", &a);
    sscanf(argv[2], "#{rfmt}", &b);
    printf("#{retfmt}", craeftfn(a, b));
    return 0;
}

|]

-- | An even simpler C harness.
--
-- Like simpleCHarness, but assumes that both argument types and the return type
-- are the same.
monomorphicCHarness cType fmt = simpleCHarness cType cType cType fmt fmt fmt

--
-- Building Craeft programs.
--

-- | Build a Craeft program which exports a single operator as a symbol.
craeftOperatorProgram ltype rtype rettype op = [i|

fn craeftfn(#{ltype} a, #{rtype} b) -> #{rettype} {
    return a #{op} b;
}

|]

-- | Like the above, but all types must be the same.
monomorphicCraeftProgram craeftType =
    craeftOperatorProgram craeftType craeftType craeftType

--
-- Representing Craeft operators for testing
--

-- | All of the information required to generate random tests for an operator.
data ArithmeticOp = ArithmeticOp {
    -- ^ The operator as applied to two identical integral arguments.
    haskellIntOp :: forall a. (Integral a, Random a, Show a) => a -> a -> a
    -- ^ The operator as applied to two identical fractional arguments.
  , haskellFracOp :: forall a. (Fractional a, Random a, Show a) => a -> a -> a
    -- ^ The Craeft name of the operator.
  , craeftOp :: String
    -- ^ The name to show in messages.
  , name :: String }

type Binary a = a -> a -> a

-- | Make an arithmetic test for a given type.
makeArithmeticTest operatorName typeName craeftTy cTy craeftOp fmt msg op =
    testCase [i|#{typeName} #{operatorName} works|] $
        programProperty (monomorphicCraeftProgram craeftTy craeftOp)
                        (monomorphicCHarness cTy fmt)
                        (\(a, b) -> [show a, show b])
                        (\(a, b) -> assertEqual (msg a b) (op a b) . read)

-- | Given an arithmetic op, generate a suite of tests for several types.
makeOperatorTestSuite :: ArithmeticOp -> TestTree
makeOperatorTestSuite (ArithmeticOp intOp floatOp craeftOp name) =
      testGroup (name ++ " tests")
    [ test "32-bit" "I32" "int32_t" "%d" (intOp :: Binary Int32)
    , test "64-bit" "I64" "int64_t" "%lld" (intOp :: Binary Int64)
    , test "32-bit unsigned" "U32" "uint32_t" "%u" (intOp :: Binary Word32) 
    , test "64-bit unsigned" "U64" "uint64_t" "%llu" (intOp :: Binary Word64) 
    , test "double" "Double" "double" "%lg" (floatOp :: Binary ApproxDouble) 
    , test "float" "Float" "float" "%g" (floatOp :: Binary ApproxFloat)  ]
  where msg a b = [i|check value of #{a} #{craeftOp} #{b}|]
        test dispName craeftName cName fmt =
          makeArithmeticTest name dispName craeftName cName craeftOp fmt msg

--
-- Approximate floating-point types.
--

-- These are somewhat hacky wrappers over floating-point types which provide in
-- addition to several useful derived instances Read instances to deal with
-- negative numbers and Eq instances that allow small differences.

newtype ApproxDouble = ApproxDouble { approxDoubleContents :: Double }
  deriving (Random, Fractional, Num, Ord)

instance Eq ApproxDouble where
  x == y = let diff = x - y
               tol = (x + y) / 100
            in abs diff <= abs tol

instance Show ApproxDouble where
  show = show . approxDoubleContents

instance Read ApproxDouble where
  readsPrec i ('-':rest) = [(ApproxDouble (-a), s) | (a, s) <- readsPrec i rest]
  readsPrec i s = [(ApproxDouble a, s) | (a, s) <- readsPrec i s]

newtype ApproxFloat = ApproxFloat { approxFloatContents :: Float }
  deriving (Random, Fractional, Num, Ord)

instance Eq ApproxFloat where
  x == y = let diff = x - y
               tol = (x + y) / 100
            in abs diff <= abs tol

instance Show ApproxFloat where
  show = show . approxFloatContents

instance Read ApproxFloat where
  readsPrec i ('-':rest) = [(ApproxFloat (-a), s) | (a, s) <- readsPrec i rest]
  readsPrec i s = [(ApproxFloat a, s) | (a, s) <- readsPrec i s]

--
-- @Random@ instance for tuples (how is this not already defined?)
--

instance (Random a, Random b) => Random (a, b) where
    randomR ((lowl, lowr), (highl, highr)) gen = ((al, ar), gr)
        where (al, gl) = randomR (lowl, highl) gen
              (ar, gr) = randomR (lowr, highr) gl
    random gen = ((left, right), genr)
        where (left, genl) = random gen
              (right, genr) = random genl
