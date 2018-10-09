{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.AutoType.CodeGen.PureScript where

import           Control.Arrow                         (first)
import           Control.Exception                     (assert)
import qualified Data.HashMap.Strict                   as Map
import           Data.Monoid                           ((<>))
import           Data.Text
import qualified Data.Text                             as T
import qualified Data.Text.IO                          as T
import           System.Exit                           (ExitCode)
import           System.FilePath
import           System.IO
import           System.Process                        (system)

import           Data.Aeson.AutoType.CodeGen.ElmFormat
import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Util

--------------------------------------------------------------------------------

-- NB: we should move common variable into separate module
--     and fill them out as config for a specific language
--     and describe CodeGen as a Class with specific set of functions

defaultFilename = "JSONTypes.purs"

header :: T.Text -> T.Text
header moduleName =
  T.unlines
    [ "import qualified Data.Text           as Text"
    , "import qualified Data.Text.IO        as Text"
    , "import           Data.Text"
    , "import qualified Data.HashMap.Strict as Map"
    , "import           Control.Arrow               (first)"
    , "import           Control.Exception (assert)"
    , "import           Data.Monoid                 ((<>))"
    , " "
    , "import           Data.Aeson.AutoType.Format"
    , "import           Data.Aeson.AutoType.Type"
    , "import           Data.Aeson.AutoType.Util"
    , "import           Data.Aeson.AutoType.CodeGen.ElmFormat"
    ]

epilogue :: Text -> Text
epilogue toplevelName = Text.unlines []

-- | Write a PureScript module to an output file, or stdout if `-` filename is given.
writeModule :: FilePath
            -> Text
            -> Map.HashMap Text Type
            -> IO ()
writeModule outputFilename toplevelName types =
  undefined

runModule :: [String] -> IO ExitCode
runModule arguments = do
  hPutStrLn stderr "Compiling PureScript module for a test."
  system $ Prelude.unwords $ ["pulp", "build", Prelude.head arguments] -- ignore parsing args
