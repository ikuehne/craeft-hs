{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification, Rank2Types #-}

module OperatorTest ( operatorTests ) where

import Debug.Trace (trace)
import Data.Int
import Data.Word

import Craeft.Parser as Parser
import Craeft.Utility
import Craeft.TypeChecker as TC
import Test.Tasty
import Test.Tasty.HUnit
import System.Random

import           Framework

cprog cop ltype rtype rettype lfmt rfmt retfmt = "#include <stdio.h>\n\
\#include <stdint.h>\n" ++ rettype ++ 
 " craeftfn(" ++ ltype ++ ", " ++ rtype ++ ");\
\int main(int argc, char **argv) {\
\    " ++ ltype ++ " a;\
\    " ++ rtype ++ " b;\
\    sscanf(argv[1], \"" ++ lfmt ++ "\", &a);\ 
\    sscanf(argv[2], \"" ++ rfmt ++ "\", &b);\ 
\    printf(\"" ++ retfmt ++ "\", craeftfn(a, b));\
\    return 0;\
\}"


data ArithOp = ArithOp {
    haskellOp :: forall a. (Num a, Random a, Show a) => a -> a -> a
  , cOp :: String
  , craeftOp :: String
}

type Binary a = a -> a -> a

makeOperatorTestSuite :: ArithOp -> TestTree
makeOperatorTestSuite (ArithOp hop cop crop) = testGroup (crop ++ " tests") [
    makeProp (hop :: Binary Int32) "32-bit" "I32" "int32_t" "%d"
  , makeProp (hop :: Binary Word32) "32-bit unsigned" "U32" "uint32_t" "%u"
  , makeProp (hop :: Binary ApproxFloat) "float" "Float" "float" "%g" ]
  where testName typeName = typeName ++ " " ++ crop ++ " works"
        craeftprog craeftty = "fn craeftfn(" ++ craeftty ++
                                           " a, " ++ craeftty ++
                                           " b) -> " ++ craeftty ++
                                           "{ return a" ++ crop ++ "b; }"
        makeCprog cty fmt = cprog cop cty cty cty fmt fmt fmt
        msg a b = "check value of " ++ show a ++ " " ++ crop ++ " " ++ show b
        makeProp hop name craeftTy cTy fmt =
            testCase (testName name) $
                programProperty (craeftprog craeftTy) (makeCprog cTy fmt)
                                (\(a, b) -> [show a, show b])
                                (\(a, b) -> assertEqual (msg a b) (hop a b)
                                          . read)

ops :: [ArithOp]
ops = [ ArithOp (+) "+" "+" ]

operatorTests = testGroup "operator tests" $ makeOperatorTestSuite <$> ops

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
-- Other type tricks we need.
--

instance (Random a, Random b) => Random (a, b) where
    randomR ((lowl, lowr), (highl, highr)) gen = ((al, ar), gr)
        where (al, gl) = randomR (lowl, highl) gen
              (ar, gr) = randomR (lowr, highr) gl
    random gen = ((left, right), genr)
        where (left, genl) = random gen
              (right, genr) = random genl
