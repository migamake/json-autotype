{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Wrappers for generating prologue and epilogue code in Haskell.
module Data.Aeson.AutoType.CodeGen.Elm(
    defaultElmFilename
  , writeElmModule
  , runElmModule
  ) where

import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           Data.Text
import qualified Data.HashMap.Strict as Map
import           Control.Arrow               (first)
import           Control.Exception (assert)
import           Data.Monoid                 ((<>))
import           System.FilePath
import           System.IO

import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Util
import           Data.Aeson.AutoType.CodeGen.ElmFormat

import Debug.Trace(trace)

defaultElmFilename = "JSONTypes.elm"

header :: Text -> Text
header moduleName = Text.unlines [
   Text.concat ["module ", capitalize moduleName]
  ,""
  ,"import Json.Encode exposing (..)"
  ,"import Json.Decode exposing (..)"
  ,"import Json.Decode.Pipeline exposing (..)"
  ,""]

epilogue :: Text -> Text
epilogue toplevelName = Text.unlines []

-- | Write a Haskell module to an output file, or stdout if `-` filename is given.
writeElmModule :: FilePath -> Text -> Map.HashMap Text Type -> IO ()
writeElmModule outputFilename toplevelName types =
    withFileOrHandle outputFilename WriteMode stdout $ \hOut ->
      assert (trace extension extension == ".elm") $ do
        Text.hPutStrLn hOut $ header $ Text.pack moduleName
        -- We write types as Haskell type declarations to output handle
        Text.hPutStrLn hOut $ displaySplitTypes types
        Text.hPutStrLn hOut $ epilogue toplevelName
  where
    (moduleName, extension) =
       first normalizeTypeName'     $
       splitExtension               $
       if     outputFilename == "-"
         then defaultElmFilename
         else outputFilename
    normalizeTypeName' = Text.unpack . normalizeTypeName . Text.pack

runElmModule = error "Yet undefined!"
