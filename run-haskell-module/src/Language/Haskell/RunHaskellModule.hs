-- | Functions for running generated modules
--
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
module Language.Haskell.RunHaskellModule
    ( RunOptions(..)
    , compileHaskellModule
    , compileHaskellModule'
    , runHaskellModule
    , runHaskellModule'
    ) where


import           Control.Exception
import           Control.Monad
import           Data.Char
import           Data.Default
import           Data.Semigroup((<>))
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.IO
import           System.Process


data RunOptions = RunOptions
        { verbose            :: Bool
        , showStdout         :: Bool
        , compileArgs        :: [String]
        , additionalPackages :: [String]
        }


instance Default RunOptions where
    def = RunOptions { verbose            = False
                     , showStdout         = False
                     , compileArgs        = []
                     , additionalPackages = []
                     }


data GhcTool = Runner | Compiler


-- | Call specified process with args and print its output when it fails.
--
callProcess' :: RunOptions -> FilePath -> [String] -> IO ExitCode
callProcess' RunOptions{..} cmd args = do
    when verbose $ putStrLn $ "Run \"" ++ cmd ++ "\" with args: " ++ show args
    (_, pstdout, pstderr, p) <- createProcess ((proc cmd args) { std_out = if showStdout then Inherit else CreatePipe, std_err = CreatePipe })
    waitForProcess p >>= \case
        ExitSuccess -> do
            unless showStdout $ whenMaybe hClose pstdout
            whenMaybe hClose pstderr
            return ExitSuccess
        ExitFailure r -> do
            whenMaybe (dumpHandle stderr) pstdout
            whenMaybe (dumpHandle stderr) pstderr
            fail $ concat ["Running \"", cmd, "\" \"", show args, "\" has failed with \"", show r, "\""]
  where
    dumpHandle outhndl inhnd = hGetContents inhnd >>= hPutStr outhndl
    whenMaybe a m = maybe (return ()) a m


-- | Splits commandline-like strings into "words", i.e.
--   ```
--   -O0 --ghc-arg="-package scientific" --ghc-arg="-package xml-typelift"
--   ```
--   transformed into 3 "words":
--   ```
--   -O0
--   --ghc-arg="-package scientific"
--   --ghc-arg="-package xml-typelift"
--   ```
splitWithQuotes :: String -> [String]
splitWithQuotes [] = []
splitWithQuotes (ch:cs)
  | isSpace ch = splitWithQuotes $ dropWhile isSpace cs
  | otherwise = word : splitWithQuotes strrest
  where
    (word, strrest) = takeWordOrQuote (ch:cs)
    takeWordOrQuote :: String -> (String, String)
    takeWordOrQuote str = let (w', rest) = takeWordOrQuote' "" False str in (reverse w', rest)
      where
        takeWordOrQuote' acc _     ""         = (acc, "")
        takeWordOrQuote' acc True  ('"':"")   = (acc, "")
        takeWordOrQuote' acc True  ('"':c:rest)
          | isSpace c = ('"':acc, rest)
          | otherwise = takeWordOrQuote' ('"':acc) False (c:rest)
        takeWordOrQuote' acc True  (c  :rest) = takeWordOrQuote' (c:acc) True rest
        takeWordOrQuote' acc False ('"':rest) = takeWordOrQuote' ('"':acc) True rest
        takeWordOrQuote' acc False (c  :rest)
          | isSpace c = (acc, rest)
          | otherwise = takeWordOrQuote' (c:acc) False rest


findGhc :: RunOptions
        -> GhcTool
        -> IO (FilePath, [String]) -- ^ returns (exe, special tool arguments)
findGhc RunOptions{..} ghcTool = do
    when verbose $ do
        let showEnv env = lookupEnv env >>= (\e -> putStrLn $ ">>> " ++ show env ++ " = " ++ show e)
        showEnv "STACK_EXE"
        showEnv "CABAL_SANDBOX_CONFIG"
        showEnv "GHC_ENVIRONMENT"
        showEnv "GHC_PACKAGE_PATH"
        showEnv "HASKELL_DIST_DIR"
        showEnv "CI_GHC_ADDITIONAL_FLAGS"
        showEnv "CI_GHC_ADDITIONAL_PACKAGES"
        showEnv "CI_GHC_CABAL_STYLE"
        -- putStrLn "Environment: -----------"
        -- getEnvironment >>= (mapM_ $ \(env,val) -> putStrLn $ env ++ " = " ++ val)
        -- putStrLn "End of environment -----"
    stack    <- lookupEnv "STACK_EXE"
    oldCabal <- lookupEnv "CABAL_SANDBOX_CONFIG"
    newCabal <- lookupEnv "HASKELL_DIST_DIR"
    additionalFlags    <- (maybe [] splitWithQuotes)                       <$> lookupEnv "CI_GHC_ADDITIONAL_FLAGS"
    additionalPackagesList <- ((additionalPackages ++) . (maybe [] words))  <$> lookupEnv "CI_GHC_ADDITIONAL_PACKAGES"
    cabalStyle         <- (maybe "v2" id)                                  <$> lookupEnv "CI_GHC_CABAL_STYLE"
    let cabalExec = cabalStyle ++ "-exec"
    let additionalPackagesArgs = map mkAdditionalPackagesArg additionalPackagesList

    let res@(exe, exeArgs') | Just stackExec <- stack    = (stackExec, additionalFlags ++ [tool, "--"])
                            | Just _         <- oldCabal = ("cabal", ["exec", tool, "--"])
                            | Just _         <- newCabal = ("cabal", [cabalExec, tool, "--"] ++ additionalPackagesArgs)
                            | otherwise                  = (tool, [])
        exeArgs = case ghcTool of
                    Compiler -> exeArgs' ++ ["-O0"]
                    Runner   -> exeArgs'
    when verbose $ putStrLn $ "Use exe \"" ++ exe ++ "\", and additional arguments: " ++ show exeArgs
    return res
  where
    tool = case ghcTool of
               Runner   -> "runghc"
               Compiler -> "ghc"
    mkAdditionalPackagesArg arg = case ghcTool of
               Runner   -> "--ghc-arg=-package=" ++ arg
               Compiler ->           "-package=" ++ arg


passModuleToGhc :: RunOptions -> GhcTool -> FilePath -> [String] -> IO ExitCode
passModuleToGhc ro ghcTool moduleFilename args =
    handle (\(e::SomeException) -> do print e >> throw e) $ do
        (exe, exeArgs) <- findGhc ro ghcTool
        callProcess' ro exe (exeArgs ++ moduleFilename:args)


-- | Find ghc with cabal or stack and compile Haskell module with specified arguments and run options
--
compileHaskellModule' :: RunOptions -> FilePath -> [String] -> IO ExitCode
compileHaskellModule' ro moduleFilename args = passModuleToGhc ro Compiler moduleFilename args

-- | Find ghc with cabal or stack and compile Haskell module with specified arguments
--
compileHaskellModule :: FilePath -> [String] -> IO ExitCode
compileHaskellModule = compileHaskellModule' def

-- | Find ghc with cabal or stack and run Haskell module in specified file with arguments and run options
--
runHaskellModule' :: RunOptions -> FilePath -> [String] -> IO ExitCode
runHaskellModule' ro moduleFilename args = passModuleToGhc ro Runner moduleFilename args


-- | Find ghc with cabal or stack and run Haskell module in specified file with arguments
--
runHaskellModule :: FilePath -> [String] -> IO ExitCode
runHaskellModule = runHaskellModule' def


