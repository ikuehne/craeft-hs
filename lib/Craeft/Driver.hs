{-|
Module      : Craeft.Driver
Description : Driving the compiler: compiling LLVM @Module@s to output files.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

module Craeft.Driver ( compileToObject
                     , compileToLlvm
                     , optimize ) where

import           Control.Monad ( void )
import           System.IO ( FilePath )

import qualified LLVM.AST as AST
import qualified LLVM.Module as LLVM
import qualified LLVM.PassManager as Pass
import qualified LLVM.Transforms as Passes
import           LLVM.Internal.Context ( withContext )
import qualified LLVM.Internal.Target as Target

-- | Run the given action on a module in a fresh context.
runTransformationInContext :: AST.Module
                           -> (LLVM.Module -> IO a)
                           -> IO a
runTransformationInContext ast op =
    withContext $ \ctx -> LLVM.withModuleFromAST ctx ast op

-- | Compile the given module to a static object file at the given path.
compileToObject :: FilePath -> AST.Module -> IO ()
compileToObject path mod =
    Target.withHostTargetMachine $ \machine ->
        runTransformationInContext mod $ \mod -> do
            -- optimize mod
            LLVM.writeObjectToFile machine (LLVM.File path) mod

optimize :: LLVM.Module -> IO ()
optimize mod = void $ Pass.withPassManager spec $ flip Pass.runPassManager mod
  where spec = Pass.defaultCuratedPassSetSpec { Pass.optLevel=Just 1 }
        passes = [ Passes.PromoteMemoryToRegister
                 , Passes.ConstantPropagation
                 , Passes.DeadInstructionElimination ]

-- | Compile the given module to LLVM IR assembly at the given path.
compileToLlvm :: FilePath -> AST.Module -> IO ()
compileToLlvm path mod =
    runTransformationInContext mod $ \mod -> do
        optimize mod
        LLVM.writeLLVMAssemblyToFile (LLVM.File path) mod
