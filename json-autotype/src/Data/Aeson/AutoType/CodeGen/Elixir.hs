{-# LANGUAGE CPP               #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Wrappers for generating prologue and epilogue code in Elixir.
module Data.Aeson.AutoType.CodeGen.Elixir(
    writeElixirModule
  , defaultElixirFilename
  , importedModules
  , requiredPackages
  , generateModuleImports
  , ModuleImport
  ) where

import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           Data.Text hiding (unwords)
import qualified Data.HashMap.Strict as Map
import           Control.Arrow               (first)
import           Control.Exception (assert)
import           Data.Default
import           Data.Monoid                 ((<>))
import           System.FilePath
import           System.IO
import           System.Process                 (system)
import qualified System.Environment             (lookupEnv)
import           System.Exit                    (ExitCode)

import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.CodeGen.Generic(src)
import           Data.Aeson.AutoType.CodeGen.ElixirFormat
import           Data.Aeson.AutoType.Util

-- | Default output filname is used, when there is no explicit output file path, or it is "-" (stdout).
-- Default module name is consistent with it.
defaultElixirFilename :: FilePath
defaultElixirFilename = "JSONTypes.ex"

-- | Generate module header
header :: Text -> Text
header _moduleName = ""

-- | Alias for indicating that this is item in module imports list.
type ModuleImport = Text

-- | Given a list of imports, generate source code.
generateModuleImports :: [ModuleImport] -> Text
generateModuleImports  = Text.unlines
                       . fmap ("import " <>)

-- | List of packages required by modules below.
--   Keep and maintain together.
requiredPackages :: [Text]
requiredPackages = ["aeson", "json-alt", "base", "bytestring", "text"]

-- | List of modules to import
importedModules :: [ModuleImport]
importedModules = []

-- | Epilogue for generated code:
--
--   * function to use parser to get data from `Text`
--   * main function in case we use `runghc` for testing parser immediately
epilogue :: Text -> Text
epilogue _toplevelName = ""

-- | Write a Elixir module to an output file, or stdout if `-` filename is given.
writeElixirModule :: FilePath -> Text -> Map.HashMap Text Type -> IO ()
writeElixirModule outputFilename toplevelName types =
    withFileOrHandle outputFilename WriteMode stdout $ \hOut ->
      assert (extension == ".ex" || extension == ".exs") $ do
        Text.hPutStrLn hOut $ header $ toplevelName
        -- We write types as Elixir type declarations to output handle
        Text.hPutStrLn hOut $ displaySplitTypes types
        Text.hPutStrLn hOut $ epilogue toplevelName
  where
    (moduleName, extension) =
       first normalizeTypeName'     $
       splitExtension               $
       if     outputFilename == "-"
         then defaultElixirFilename
         else outputFilename
    normalizeTypeName' = Text.unpack . normalizeTypeName . Text.pack
