
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.AutoType.CodeGen.PureScript(
    defaultPureScriptFilename
  , writePureScriptModule
  , runPureScriptModule
) where

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
import           Debug.Trace(trace)

import           Data.Aeson.AutoType.CodeGen.PureScriptFormat
import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Util

--------------------------------------------------------------------------------

-- NB: SB: In future, we should move common variable into separate module
--     and fill them out as config for a specific language
--     and describe CodeGen as a Class with specific set of functions
--     like: defaultFilename, header, footer(instead of epilogie), 
--     writeModule, runModule
--     without Langauge identification in all this functions

defaultPureScriptFilename = "JSONTypes.purs"

-- | Top level declarations that imports 3rd party libraries
-- 
header :: T.Text -> T.Text
header moduleName =
  T.unlines
    [ T.unwords ["module ", capitalize moduleName, " where"]
    ,""
    , "import Prelude"
    , "import Control.Monad.Eff"
    , "import Control.Monad.Eff.Console"
    , "import qualified Data.String as String"
    , "import qualified Node.Process as Process"
    , "import Data.Argonaut (Json, decodeJson, encodeJson, stringify) as A"
    , "import Data.Argonaut.Gen (genJson) as A"
    , "import Data.Argonaut.JCursor (JCursor(..), toPrims, fromPrims) as A"
    , "import Data.Either (Either(..))"
    , "import Data.Foldable (foldMap)"
    , "import Data.Maybe (Maybe(..))"
    , ""
    ]

-- | Additional top level functions that needed for autonomous execution
-- 
epilogue :: T.Text -> T.Text
epilogue toplevelName = 
  T.unlines 
    [ "doWithArgv :: forall e. (Array String -> Eff (e) Unit) -> Eff (process :: Process.PROCESS | e) Unit"
    , "doWithArgv f = do"
    , "  args <- Process.argv"
    , "  (f args)"
    , "  return unit"
    , " "
    , "printSplitArgs :: forall e. Array String -> Eff (console :: CONSOLE | e) Unit"
    , "printSplitArgs argv = do"
    , "  print (String.joinWith “,” argv)"
    , " "
    , "main :: forall e. Eff (console :: CONSOLE, process :: Process.PROCESS | e) Unit"
    , "main = do"
    , "  doWithArgv printSplitArgs"
    ]

-- | Write a PureScript module to an output file
--   or stdout if `-` filename is given.
writePureScriptModule :: FilePath               -- ^ path to output file
                      -> T.Text                 -- ^ top level names
                      -> Map.HashMap Text Type  -- ^ used types
                      -> IO ()
writePureScriptModule outputFilename toplevelName types =
    withFileOrHandle outputFilename WriteMode stdout $ \hOut ->
    assert (trace extension extension == ".purs") $ do
      T.hPutStrLn hOut $ header $ T.pack moduleName
      --T.hPutStrLn hOut $ displaySplitTypes types
      T.hPutStrLn hOut $ epilogue toplevelName
  where
    (moduleName, extension) =
       first normalizeTypeName' $
         splitExtension         $
           (case outputFilename == "-" of
             False -> outputFilename
             True  -> defaultPureScriptFilename)
    normalizeTypeName' = T.unpack . normalizeTypeName . T.pack


runPureScriptModule :: [String]    -- ^ Arguments for build/run tool, pulp
                    -> IO ExitCode
runPureScriptModule arguments = do
  hPutStrLn stderr "Compiling PureScript module for a test."
  system $ Prelude.unwords $ ["pulp", "build", Prelude.head arguments]
