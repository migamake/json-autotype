{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Code generation and test running in different languages. (Switchbox.)
module Data.Aeson.AutoType.CodeGen(
    Lang(..)
  , writeModule
  , runModule
  , defaultOutputFilename
  ) where

import           Data.Text(Text)
import qualified Data.HashMap.Strict as Map
import           Data.Aeson.AutoType.Type

import           Data.Aeson.AutoType.CodeGen.Haskell
import           Data.Aeson.AutoType.CodeGen.Elm
import           Data.Aeson.AutoType.CodeGen.PureScript

-------------------------------------------------------------------------------

-- | Available output languages.
data Lang = Haskell
          | HaskellStrict
          | Elm
          | PureScript

-- | Default output filname is used, when there is no explicit output file path, or it is "-" (stdout).
--   Default module name is consistent with it.
defaultOutputFilename :: Lang -> FilePath
defaultOutputFilename Haskell       = defaultHaskellFilename
defaultOutputFilename HaskellStrict = defaultHaskellFilename
defaultOutputFilename Elm           = defaultElmFilename
defaultOutputFilename PureScript    = defaultPureScriptFilename

-- | Write a Haskell module to an output file,
--   or stdout if `-` filename is given.
writeModule :: Lang                   -- ^ output language type
            -> FilePath               -- ^ path to the file 
            -> Text                   -- ^ top level names
            -> Map.HashMap Text Type  -- ^ used types
            -> IO ()
writeModule Haskell       = writeHaskellModule
writeModule HaskellStrict = writeHaskellModule
writeModule Elm           = writeElmModule
writeModule PureScript    = writePureScriptModule

-- | Run module in a given language.
runModule Haskell       = runHaskellModule
runModule HaskellStrict = runHaskellModuleStrict
runModule Elm           = runElmModule
runModule PureScript    = runPureScriptModule
