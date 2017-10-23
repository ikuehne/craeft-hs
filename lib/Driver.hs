{-|
Module      : Driver
Description : Driving the compiler: compiling LLVM @Module@s to output files.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

module Driver ( compileToObject
              , compileToLlvm ) where

import System.IO ( FilePath )

import qualified LLVM.AST as AST
import qualified LLVM.Module as LLVM
import LLVM.Internal.Context ( withContext )
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
        runTransformationInContext mod $
            LLVM.writeObjectToFile machine (LLVM.File path)

-- | Compile the given module to LLVM IR assembly at the given path.
compileToLlvm :: FilePath -> AST.Module -> IO ()
compileToLlvm path mod =
    runTransformationInContext mod $
        LLVM.writeLLVMAssemblyToFile (LLVM.File path)
