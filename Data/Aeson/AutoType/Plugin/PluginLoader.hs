{-# LANGUAGE ScopedTypeVariables #-}
-- | Module implementing loading generic plugins using Hint.
module LoadPlugin(
    importPlugins
  , main
  ) where

import           System.IO     (hPutStrLn, stderr)
import           System.Exit   (exitFailure)
import           Data.Typeable
import           Control.Arrow ((&&&))
import           Control.Monad
import qualified Language.Haskell.Interpreter as Hint

-- | Test script
main :: IO ()
main  = do result :: [[Int]] <- importPlugins "export" moduleNames
           forM_ (zip moduleNames result) $ \(moduleName, exportVal) ->
             putStrLn $ concat [moduleName, ": ", show exportVal, "\n"]
  where
    moduleNames = ["Plugin.hs"
                  ,"Plugin2.hs"]

-- | Fatal exit from the program
fatal :: String -> IO a
fatal msg = do
  hPutStrLn stderr msg
  exitFailure

-- | Imports a set of plugin modules with the same "interface" value,
-- and returns values exported as their interfaces.
importPlugins ::   Typeable a =>
                   String     -> -- ^ name exported from each module as an interface
                  [String]    -> -- ^ list of plugin modules (given as either module paths or file paths)
                   IO [a]        -- ^ Result is a list of exported objects in the order of module names
importPlugins interfaceName pluginModules = do
    result <- Hint.runInterpreter $ do
      Hint.loadModules pluginModules
      moduleNames <- Hint.getLoadedModules
      Hint.setImportsQ $ ("Prelude", Nothing):map (id &&& Just) moduleNames
      mapM getInterfaceVar moduleNames
    case result of
      Left  err   -> fatal $ "Cannot load plugins:\n" ++ show err
      Right value -> return value
  where
    getInterfaceVar moduleName = Hint.interpret (moduleName ++ "." ++ interfaceName) Hint.as

