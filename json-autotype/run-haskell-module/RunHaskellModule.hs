-- | Functions for running generated modules
--
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module RunHaskellModule
    ( RunOptions(..)
    , compileHaskellModule
    , runHaskellModule
    , runHaskellModule'
    ) where


import           Control.Exception
import           Control.Monad
import           Data.Default
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Process


data RunOptions = RunOptions
        { verbose     :: Bool
        , showStdout  :: Bool
        , compileArgs :: [String]
        }


instance Default RunOptions where
    def = RunOptions { verbose     = False
                     , showStdout  = False
                     , compileArgs = []
                     }


data GhcTool = Runner | Compiler


-- | Call specified process with args and print its output when it fails.
--
callProcess' :: RunOptions -> FilePath -> [String] -> IO ExitCode
callProcess' RunOptions{..} cmd args = do
    (_, pstdout, pstderr, p) <- createProcess ((proc cmd args) { std_out = if showStdout then Inherit else CreatePipe, std_err = CreatePipe })
    waitForProcess p >>= \case
        ExitSuccess -> do
            -- whenMaybe hClose pstdout
            whenMaybe hClose pstderr
            return ExitSuccess
        e@(ExitFailure r) -> do
            whenMaybe (dumpHandle stdout) pstdout
            whenMaybe (dumpHandle stderr) pstderr
            fail $ concat ["Running \"", cmd, "\" \"", show args, "\" has failed with \"", show r, "\""]
            -- return e
  where
    dumpHandle outhndl inhnd = hGetContents inhnd >>= hPutStr outhndl
    whenMaybe a m = maybe (return ()) a m


findGhc :: RunOptions
        -> GhcTool
        -> IO (FilePath, [String]) -- ^ returns (exe, special tool arguments)
findGhc RunOptions{..} ghcTool = do
    stack <- lookupEnv "STACK_EXE"
    cabal <- lookupEnv "CABAL_SANDBOX_CONFIG"
    when verbose $ putStrLn $ concat ["STACK_EXE=", show stack, "; CABAL_SANDBOX_CONFIG=", show cabal]
    let res@(exe, exeArgs') | Just stackExec <- stack = (stackExec, [tool] ++ compileArgs ++ ["--"])
                            | Just _         <- cabal = ("cabal",   ["exec", tool] ++ compileArgs ++ ["--"])
                            | otherwise               = (tool,      compileArgs)
        exeArgs = case ghcTool of
                    Compiler -> exeArgs' ++ ["-O0"]
                    Runner   -> exeArgs
    when verbose $ putStrLn $ concat ["Use exe \"", exe, "\", and additional arguments: \"", show exeArgs]
    return res
  where
    tool = case ghcTool of
               Runner   -> "runghc"
               Compiler -> "ghc"


passModuleToGhc :: RunOptions -> GhcTool -> FilePath -> [String] -> IO ExitCode
passModuleToGhc ro ghcTool moduleFilename args =
    handle (\(e::SomeException) -> do print e >> throw e) $ do
        (exe, exeArgs) <- findGhc ro ghcTool
        callProcess' ro exe (exeArgs ++ moduleFilename:args)


-- | Find ghc with cabal/stack and run it with specified arguments
--
compileHaskellModule :: FilePath -> [String] -> IO ExitCode
compileHaskellModule moduleFilename args = passModuleToGhc def Compiler moduleFilename args


-- | Run Haskell module in specified file with arguments
--
runHaskellModule' :: RunOptions -> FilePath -> [String] -> IO ExitCode
runHaskellModule' ro moduleFilename args = passModuleToGhc ro Runner moduleFilename args


runHaskellModule :: FilePath -> [String] -> IO ExitCode
runHaskellModule moduleFilename args = runHaskellModule' def moduleFilename args


