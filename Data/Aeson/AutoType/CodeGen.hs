{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Wrappers for generating prologue and epilogue code in Haskell.
module Data.Aeson.AutoType.CodeGen(
    Lang(..)
  , writeModule
  , runModule
  , defaultOutputFilename
  ) where

import           Data.Text(Text)
import qualified Data.HashMap.Strict as Map
import           Data.Aeson.AutoType.Type
{-
import qualified Data.Text.IO        as Text
import           Data.Text
import qualified Data.HashMap.Strict as Map
import           Control.Arrow               (first)
import           Data.Monoid                 ((<>))
import           System.FilePath
import           System.IO

import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.Util -}
import           Data.Aeson.AutoType.CodeGen.Haskell
import           Data.Aeson.AutoType.CodeGen.Elm

data Lang = Haskell
          | Elm

-- | Default output filname is used, when there is no explicit output file path, or it is "-" (stdout).
-- Default module name is consistent with it.
defaultOutputFilename :: Lang -> FilePath
defaultOutputFilename Haskell = defaultHaskellFilename
defaultOutputFilename Elm     = defaultElmFilename

-- | Write a Haskell module to an output file, or stdout if `-` filename is given.
writeModule :: Lang -> FilePath -> Text -> Map.HashMap Text Type -> IO ()
writeModule Haskell = writeHaskellModule
writeModule Elm     = writeElmModule

runModule Haskell = runHaskellModule
runModule Elm     = runElmModule
