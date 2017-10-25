module Main ( main ) where

import           Debug.Trace (traceM)
import           System.Environment ( getArgs )
import           System.IO ( FilePath )
import qualified System.IO as IO
import           System.Exit ( exitWith, ExitCode (..) )
import qualified System.Console.GetOpt as GetOpt

import           Control.Monad.Trans.Except
import qualified LLVM.AST as AST

import qualified Craeft.Driver as Driver
import qualified Craeft.Parser as Parser
import           Craeft.Utility
import qualified Craeft.TypeChecker as TC
import qualified Craeft.Codegen as Codegen

main :: IO ()
main = do args <- getArgs
          (inpath, opts) <- handleExcept $ compilerOpts args
          compiled <- compile inpath
          print compiled
          mapM_ (handleOpt compiled) opts

-- | Read in a file and translate it to a pure LLVM module.
compile :: FilePath -> IO AST.Module
compile path = do contents <- IO.readFile path
                  handleExcept $ do
                      parsed <- Parser.parseProgram path contents
                      checked <- TC.typeCheck parsed
                      Codegen.codegen checked

--
-- @GetOpt@-based command line argument handling.
--

-- | Possible flags.
data Opt = IROutput FilePath
         | ObjOutput FilePath

-- | Syntax for inputting those flags.
options :: [GetOpt.OptDescr Opt]
options = [ GetOpt.Option ['l']  ["llvm"] (GetOpt.ReqArg IROutput "FILE")
                                          "output path for LLVM IR"
          , GetOpt.Option ['c']  ["obj"]  (GetOpt.ReqArg ObjOutput "FILE")
                                          "output path for object code" ]

-- | Run the action corresponding to a single command-line option.
handleOpt :: AST.Module -> Opt -> IO ()
handleOpt mod (IROutput path) = Driver.compileToLlvm path mod
handleOpt mod (ObjOutput path) = Driver.compileToObject path mod

-- | Retrieve the compiler instructions: an input path, and a series of ouptuts.
compilerOpts :: [String] -> CraeftExcept (FilePath, [Opt])
compilerOpts argv = 
    case GetOpt.getOpt GetOpt.Permute options argv of
       (o, [inpt], []) -> return (inpt, o)
       (o, _, []) -> throwE $ UsageError $ firstLine ++ "\n\nNo input file."
       (_, _, errs) -> throwE $ UsageError $ header
                                          ++ "\n\nErrors:\n" ++ concat errs
  where firstLine = "usage: craeftc input_file [option...]"
        header = GetOpt.usageInfo firstLine options

--
-- Handling errors.
--

-- | Print an error and exit, or return the result.
handleExcept :: CraeftExcept a -> IO a
handleExcept e = case runExcept e of
                     Left err -> do prettyPrintError err
                                    exitWith $ ExitFailure 1
                     Right res -> return res

