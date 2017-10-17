{-|
Module      : Driver
Description : Driving the compiler: compiling LLVM @Module@s to output files.
Copyright   : (c) Ian Kuehne, 2017
License     : GPL-3
Maintainer  : ikuehne@caltech.edu
Stability   : experimental
-}

module Driver where

import System.IO ( FilePath )

import qualified LLVM.AST as AST
import qualified LLVM.Module as LLVM
import LLVM.Internal.Context ( withContext )
import qualified LLVM.Internal.Target as Target

runTransformationInContext :: AST.Module
                           -> (LLVM.Module -> IO a)
                           -> IO a
runTransformationInContext ast op =
    withContext $ \ctx -> LLVM.withModuleFromAST ctx ast op

compileToObject :: FilePath -> AST.Module -> IO ()
compileToObject path mod =
    Target.withHostTargetMachine $ \machine ->
        runTransformationInContext mod $ \llmod ->
            LLVM.writeObjectToFile machine (LLVM.File path) llmod

compileToLlvm :: FilePath -> AST.Module -> IO ()
compileToLlvm path mod =
    runTransformationInContext mod $ \llmod ->
        LLVM.writeLLVMAssemblyToFile (LLVM.File path) llmod
