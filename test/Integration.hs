module Integration (programProperty) where

import           Control.Exception
import           Control.Monad (forM_, when)
import           Control.Monad.Trans.Except
import qualified Craeft.Driver as Driver
import qualified Craeft.Parser as Parser
import           Craeft.Utility
import qualified Craeft.TypeChecker as TC
import qualified Craeft.Codegen as Codegen
import qualified LLVM.AST as AST
import           Test.Tasty.HUnit
import           System.IO
import qualified System.IO.Temp as Temp
import qualified System.Process as Proc
import qualified System.Random as Rand

programProperty :: (Show a, Rand.Random a)
                => String -- ^ A Craeft program to test.
                -> String -- ^ A C program to link it against.
                -> (a -> [String]) -- ^ A way to generate arguments
                -- ^ The property of the output text to test.
                -> (a -> String -> Assertion)
                -> Assertion
programProperty craeftProgram cProgram makeArgs test =
    withCompiledCraeftExecutable craeftProgram cProgram $ \execfile ->
        forM_ [1..100] $ \args -> do
            testCase <- Rand.randomIO
            stdout <- Proc.readProcess execfile (makeArgs testCase) ""
            test testCase stdout

compileProgram :: String -> CraeftExcept AST.Module
compileProgram program = Parser.parseProgram "[test]" program
                     >>= TC.typeCheck
                     >>= Codegen.codegen

withCompiledCProgram :: String -- ^ The text of the C program to compile.
                     -- ^ The operation to run on the C program.
                     -> (FilePath -> IO a)
                     -> IO a
withCompiledCProgram program action =
    Temp.withSystemTempFile "harness.o" $ \objfile objhandle -> do
        hClose objhandle
        Proc.readProcess "gcc" ["-xc", "-", "-c", "-o", objfile] program
        action objfile

withCompiledCraeftProgram :: String
                          -> (FilePath -> IO a)
                          -> IO a
withCompiledCraeftProgram prog action =
      Temp.withSystemTempFile "craeftobj.o" $ \objfile objhandle -> do
          hClose objhandle
          c <- compiled
          Driver.compileToObject objfile c
          action objfile
    where compiled = case runExcept $ compileProgram prog of
              Left err -> throw err
              Right res -> return res

withCompiledCraeftExecutable :: String -- ^ Craeft program.
                             -> String -- ^ C harness.
                             -> (FilePath -> IO a)
                             -> IO a
withCompiledCraeftExecutable craeftprogText cprogText action =
    withCompiledCProgram cprogText $ \cprog ->
    withCompiledCraeftProgram craeftprogText $ \craeftprog ->
    Temp.withSystemTempFile "craeftexec.bin" $ \execfile exechandle -> do
        hClose exechandle
        Proc.callProcess "gcc" [cprog, craeftprog, "-o", execfile]
        action execfile
