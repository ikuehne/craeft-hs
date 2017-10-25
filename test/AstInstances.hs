module AstInstances where

import Control.Monad ( liftM2, liftM3 )

import Craeft.AST
import Craeft.Utility
import Test.Tasty.QuickCheck as QC

import Utility

instance Arbitrary FunctionSignature where
  arbitrary = liftM3 FunctionSignature arbitrary arbitrary arbitrary

instance Arbitrary ValueDeclaration where
    arbitrary = liftM2 ValueDeclaration arbitrary arbitrary

instance Arbitrary Type where
    arbitrary = do
        i <- arbitrary
        case abs i `rem` (3 :: Int) of
            0 -> fmap NamedType arbitrary
            1 -> return Void
            2 -> fmap Pointer arbitrary

instance Arbitrary TopLevel where
    arbitrary = do
        i <- arbitrary
        case abs i `rem` (4 :: Int) of
            0 -> liftM2 StructDeclaration arbitrary arbitrary
            1 -> fmap TypeDeclaration arbitrary
            2 -> fmap FunctionDecl arbitrary
            3 -> liftM2 FunctionDefinition arbitrary arbitrary

instance Arbitrary Statement where
    arbitrary = do
        i <- arbitrary
        case abs i `rem` (7 :: Int) of
            0 -> fmap ExpressionStatement arbitrary
            1 -> fmap Return arbitrary
            2 -> return VoidReturn
            3 -> liftM2 Assignment arbitrary arbitrary
            4 -> fmap Declaration arbitrary
            5 -> liftM2 CompoundDeclaration arbitrary arbitrary
            6 -> liftM3 IfStatement arbitrary arbitrary arbitrary

operators = ["+", "/", "-", "+", "&&", "||", "&", ".", "->", "<:", ":>", "==",
             "!=", ">=", "<=", ">", "<", "="]         

instance Arbitrary Expression where
    arbitrary = do
        i <- arbitrary
        case abs i `rem` (9 :: Int) of
            0 -> fmap IntLiteral arbitrary
            1 -> fmap UIntLiteral arbitrary
            2 -> fmap FloatLiteral arbitrary
            3 -> fmap StringLiteral arbitrary
            4 -> fmap Reference arbitrary
            5 -> do
                lhs <- arbitrary
                op <- (operators !!)
                    . flip rem (length operators)
                    . abs
                  <$> arbitrary
                rhs <- arbitrary
                return $ Binop lhs op rhs
            6 -> liftM2 FunctionCall arbitrary arbitrary
            7 -> liftM2 Cast arbitrary arbitrary
            8 -> fmap LValueExpr arbitrary

instance Arbitrary LValue where
    arbitrary = do 
        i <- arbitrary
        case abs i `rem` (3 :: Int) of
            0 -> fmap Variable arbitrary
            1 -> fmap Dereference arbitrary
            2 -> liftM2 FieldAccess arbitrary arbitrary
