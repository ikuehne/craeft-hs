module Main where

import Debug.Trace (traceM)
import System.Environment ( getArgs )
import System.IO ( FilePath )
import qualified System.IO as IO
import System.Exit ( exitWith, ExitCode (..) )
import qualified System.Console.GetOpt as GetOpt

import qualified LLVM.AST as AST

import qualified Driver
import qualified Parser
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (evalStateT)
import Utility
import qualified Environment
import qualified TypeChecker as TC
import qualified TypedAST as TAST
import qualified Codegen

data Opt = IROutput FilePath
         | ObjOutput FilePath

options :: [GetOpt.OptDescr Opt]
options = [ GetOpt.Option ['l']  ["llvm"] (GetOpt.ReqArg IROutput "FILE")
                                          "output path for LLVM IR"
          , GetOpt.Option ['c']  ["obj"]  (GetOpt.ReqArg ObjOutput "FILE")
                                          "output path for object code" ]

main :: IO ()
main = do args <- getArgs
          (inpath, opts) <- handleExcept $ compilerOpts args
          compiled <- compile inpath
          mapM_ (handleOpt compiled) opts

handleOpt :: AST.Module -> Opt -> IO ()
handleOpt mod (IROutput path) = Driver.compileToLlvm path mod
handleOpt mod (ObjOutput path) = Driver.compileToObject path mod

compilerOpts :: [String] -> CraeftExcept (FilePath, [Opt])
compilerOpts argv = 
    case GetOpt.getOpt GetOpt.Permute options argv of
       (o, [inpt], []) -> return (inpt, o)
       (o, _, []) -> throwE $ UsageError $ firstLine ++ "\n\nNo input file."
       (_, _, errs) -> throwE $ UsageError $ header
                                          ++ "\n\nErrors:\n" ++ concat errs
  where firstLine = "usage: craeftc input_file [option...]"
        header = GetOpt.usageInfo firstLine options

handleExcept :: CraeftExcept a -> IO a
handleExcept e = case runExcept e of
                     Left err -> do prettyPrintError err
                                    exitWith $ ExitFailure 1
                     Right res -> return res

compile :: FilePath -> IO AST.Module
compile path = do contents <- IO.readFile path
                  handleExcept $ do
                      parsed <- Parser.parseProgram path contents
                      checked <- TC.typeCheck parsed
                      Codegen.codegen checked
