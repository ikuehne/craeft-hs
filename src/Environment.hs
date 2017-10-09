{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Environment ( Precision (..)
                   , Type (..)
                   , Value (..)
                   , EnvironmentState
                   , Environment
                   , initialEnv
                   , pop
                   , push
                   , lookupValue
                   , lookupType
                   , insertValue
                   , insertType ) where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State (StateT)
import qualified Data.Map as Map

import Control.Lens
import LLVM.AST.Operand (Operand)

import Error
import qualified Scope

data Precision = SinglePrec | DoublePrec
  deriving (Eq, Ord)

data Type = Struct [(String, Type)]
          | Pointer Type
          | Signed Int
          | Unsigned Int
          | Function [Type] Type
          | Floating Precision
          | Opaque
          | Void
  deriving Eq

data Value = Value { ty :: Type, value :: Operand }

signedTypes :: Map.Map String Type
signedTypes = let fromInt i = ("I" ++ show i, Signed i)
               in Map.fromList $ fromInt <$> [0..16777215]

unsignedTypes :: Map.Map String Type
unsignedTypes = let fromInt i = ("U" ++ show i, Unsigned i)
                 in Map.fromList $ fromInt <$> [0..16777215]

floatTypes :: Map.Map String Type
floatTypes = Map.fromList [ ("Float", Floating SinglePrec)
                          , ("Double", Floating DoublePrec)]

initialTypes :: Scope.ScopeState Type
initialTypes = Scope.make $ Map.unions [signedTypes, unsignedTypes, floatTypes]

initialValues :: Scope.ScopeState Value
initialValues = Scope.make Map.empty

data EnvironmentState =
    EnvironmentState { _typeScope  :: Scope.ScopeState Type
                     , _valueScope :: Scope.ScopeState Value }
makeLenses ''EnvironmentState

type Environment a = CraeftMonad EnvironmentState a

initialEnv :: EnvironmentState
initialEnv = EnvironmentState initialTypes initialValues

pop :: Environment ()
pop = zoom typeScope Scope.pop >> zoom valueScope Scope.pop

push :: Environment ()
push = zoom typeScope Scope.push >> zoom valueScope Scope.push

lookupValue :: String -> SourcePos -> Environment Value
lookupValue v pos = zoom valueScope $ Scope.lookup v pos

lookupType :: String -> SourcePos -> Environment Type
lookupType t pos = zoom typeScope $ Scope.lookup t pos

insertValue :: String -> Value -> Environment ()
insertValue s v = zoom valueScope $ Scope.insert s v

insertType :: String -> Type -> Environment ()
insertType s t = zoom typeScope $ Scope.insert s t
