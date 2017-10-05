{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Environment ( Precision (..)
                   , Type (..)
                   , Value (..)
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

import LLVM.AST.Operand (Operand)
import Text.Parsec.Pos

import Error

data Precision = SinglePrec | DoublePrec

data Type = Struct [(Type, String)]
          | Pointer Type
          | Signed Int
          | Unsigned Int
          | Floating Precision

data Value = Value { ty :: Type, value :: Operand }

newtype Scope a = Scope { scopes :: [Map.Map String a] }

signedTypes :: Map.Map String Type
signedTypes = let fromInt i = ("I" ++ show i, Signed i)
               in Map.fromList $ fromInt <$> [0..16777215]

unsignedTypes :: Map.Map String Type
unsignedTypes = let fromInt i = ("U" ++ show i, Unsigned i)
                 in Map.fromList $ fromInt <$> [0..16777215]

floatTypes :: Map.Map String Type
floatTypes = Map.fromList [ ("Float", Floating SinglePrec)
                          , ("Double", Floating DoublePrec)]

initialTypes :: Scope Type
initialTypes = Scope [Map.unions [signedTypes, unsignedTypes, floatTypes]]

initialValues :: Scope Value
initialValues = Scope [Map.empty]

pushScope :: Scope a -> Scope a
pushScope (Scope scopes) = Scope $ Map.empty : scopes

popScope :: Scope a -> CraeftExcept (Scope a)
popScope (Scope (s:ss)) = return $ Scope ss
popScope _ = throwE $ InternalError "attempt to pop empty environment"

lookupScope :: String -> SourcePos -> Scope a -> CraeftExcept a
lookupScope k p (Scope scopes) = case foldr (<|>) Nothing results of
                                    Nothing -> throwE $ NameError msg p
                                    Just x  -> return x
  where results = Map.lookup k <$> scopes
        msg = "name not found in scope: " ++ k

insertScope :: String -> a -> Scope a -> Scope a
insertScope k v (Scope (s:ss)) = Scope (Map.insert k v s:ss)

data EnvironmentState = EnvironmentState { typeScope  :: Scope Type
                                         , valueScope :: Scope Value }

newtype Environment a =
    Environment { unEnvironment :: StateT EnvironmentState CraeftExcept a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState EnvironmentState
           , MonadError Error )

initialEnv :: EnvironmentState
initialEnv = EnvironmentState initialTypes initialValues

modifyTypeScope :: (Scope Type -> Scope Type) -> Environment ()
modifyTypeScope f = modify $ \e -> e { typeScope = f $ typeScope e }

modifyValueScope :: (Scope Value -> Scope Value) -> Environment ()
modifyValueScope f = modify $ \e -> e { valueScope = f $ valueScope e }

setTypeScope :: Scope Type -> Environment ()
setTypeScope = modifyTypeScope . const

setValueScope :: Scope Value -> Environment ()
setValueScope = modifyValueScope . const

liftE :: CraeftExcept a -> Environment a
liftE = Environment . lift

pop :: Environment ()
pop = do gets typeScope >>= liftE . popScope >>= setTypeScope
         gets valueScope >>= liftE . popScope >>= setValueScope

push :: Environment ()
push = do modifyTypeScope pushScope
          modifyValueScope pushScope

lookupValue :: String -> SourcePos -> Environment Value
lookupValue v pos = gets valueScope >>= liftE . lookupScope v pos

lookupType :: String -> SourcePos -> Environment Type
lookupType t pos = gets typeScope >>= liftE . lookupScope t pos

insertValue :: String -> Value -> Environment ()
insertValue s v = modifyValueScope $ insertScope s v

insertType :: String -> Type -> Environment ()
insertType s t = modifyTypeScope $ insertScope s t
