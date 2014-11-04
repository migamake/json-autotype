{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           System.Exit
import           System.IO                 (stdin, stderr, stdout, IOMode(..), hPutStrLn)
import           System.FilePath           (splitExtension)
import           Control.Monad             (forM_, when)
import           Control.Exception(assert)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict        as Map
import           Data.Aeson
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Data.Text                 (Text)

import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Format
import           CLI
import           HFlags

fst3 ::  (t, t1, t2) -> t
fst3 (a, _, _) = a

assertM ::  Monad m => Bool -> m ()
assertM v = assert v $ return ()

capitalize :: Text -> Text
capitalize input = Text.toUpper (Text.take 1 input)
                   `Text.append` Text.drop 1 input

header :: Text -> Text
header moduleName = Text.unlines [
   "{-# LANGUAGE TemplateHaskell #-}"
  ,Text.concat ["module ", capitalize moduleName, " where"]
  ,""
  ,"import           System.Environment (getArgs)"
  ,"import           Control.Monad      (forM_)"
  ,"import           Data.ByteString.Lazy.Char8 as BSL"
  ,"import           Data.Text (Text)"
  ,"import           Data.Aeson(decode, Value(..), FromJSON(..),"
  ,"                            (.:), (.:?), (.!=))"
  ,"import           Data.Aeson.TH"
  ,""]

epilogue :: Text
epilogue          = Text.unlines
  [""
  ,"parse :: FilePath -> IO TopLevel"
  ,"parse filename = do result <- decode `fmap` BSL.readFile filename"
  ,"                    case result of"
  ,"                      Nothing -> fail $ \"Cannot parse JSON from file: \" ++ filename"
  ,"                      Just r  -> return r"
  ,""
  ,"main :: IO ()"
  ,"main = do filenames <- getArgs"
  ,"          forM_ filenames (\\f -> parse f >>= print)"
  ,""]

-- * Command line flags
defineFlag "outputFilename"  (defaultOutputFilename :: FilePath) "Write output to the given file"
defineFlag "suggest"         True                                "Suggest candidates for unification"
defineFlag "autounify"       True                                "Automatically unify suggested candidates"
defineFlag "fakeFlag"        True                                "Ignore this flag - it doesn't exist!!!"

-- Tracing is switched off:
myTrace :: String -> IO ()
myTrace _msg = return ()
--myTrace = putStrLn 

-- | Report an error to error output.
report   :: Text -> IO ()
report    = Text.hPutStrLn stderr

-- | Report an error and terminate the program.
fatal    :: Text -> IO ()
fatal msg = do report msg
               exitFailure

-- | Take a set of JSON input filenames, Haskell output filename, and generate module parsing these JSON files.
generateHaskellFromJSONs :: [FilePath] -> FilePath -> IO ()
generateHaskellFromJSONs inputFilenames outputFilename = do
  assertM (extension == ".hs")
  withFileOrHandle outputFilename WriteMode stdout $ \hOut ->
    forM_ inputFilenames $ \inputFilename ->
      withFileOrHandle inputFilename ReadMode stdin $ \hIn ->
        -- First we decode JSON input into Aeson's Value type
        do bs <- BSL.hGetContents hIn
           Text.hPutStrLn stderr $ "Processing " `Text.append` Text.pack (show moduleName)
           myTrace ("Decoded JSON: " ++ show (decode bs :: Maybe Value))
           case decode bs of
             Nothing -> report $ "Cannot decode JSON input from " `Text.append` Text.pack (show inputFilename)
             Just v  -> do -- If decoding JSON was successful...
               -- We start by writing module header
               Text.hPutStrLn hOut $ header $ Text.pack moduleName
               -- We extract type structure from the JSON value.
               let t        = extractType v
               myTrace $ "type: " ++ show t
               -- We split different dictionary labels to become different type trees (and thus different declarations.)
               let splitted = splitTypeByLabel "TopLevel" t
               myTrace $ "splitted: " ++ show splitted
               assertM $ not $ any hasNonTopTObj $ Map.elems splitted
               -- We compute which type labels are candidates for unification
               let uCands = unificationCandidates splitted
               myTrace $ "candidates: " ++ show uCands
               when flags_suggest $ forM_ uCands $ \cs -> do
                                      putStr "-- "
                                      Text.putStrLn $ "=" `Text.intercalate` cs
               -- We unify the all candidates or only those that have been given as command-line flags.
               let unified = if flags_autounify
                               then unifyCandidates uCands splitted
                               else splitted
               myTrace $ "unified: " ++ show unified
               -- We write types as Haskell type declarations to output handle
               Text.hPutStrLn hOut $ displaySplitTypes unified
               Text.hPutStrLn hOut   epilogue
  where
    (moduleName, extension) = splitExtension $
                                if     outputFilename == "-"
                                  then defaultOutputFilename
                                  else outputFilename
generateJSON _               outputFilename = error "Generating common type from multiple input files is not implemented yet!"

main :: IO ()
main = do filenames <- $initHFlags "json-autotype -- automatic type and parser generation from JSON"
          -- TODO: should integrate all inputs into single type set!!!
          generateHaskellFromJSONs filenames flags_outputFilename
