module TypeCheckerTest where

import Data.Either (isLeft, isRight)

import Control.Monad.Except
import Craeft.Utility
import Craeft.AST
import Craeft.TypeChecker
import Craeft.Scope as Scope
import Test.Tasty
import Test.Tasty.HUnit

import Utility

--
-- Testing failures.
--

funcWithBody = FunctionDefinition $
    annotate (FunctionSignature "func" [] [] (annotate Void))

shouldSucceed str program = testCase ("Typechecker fails on " ++ str) $
    case runExcept . typeCheck $ map annotate program of
        Left err -> assertFailure (renderError err) 
        Right _ -> return ()

shouldFail str program = testCase ("Typechecker does not fail on " ++ str) $
    assertBool str . isLeft . runExcept . typeCheck $ map annotate program

undefinedTypenameFails = shouldFail "undefined type name" [
    funcWithBody $ map annotate [
        Declaration (ValueDeclaration (NamedType "Undefined" []) "x") ] ]

typeCheckerTests = testGroup "TypeChecker Tests" [
        undefinedTypenameFails
    ]
