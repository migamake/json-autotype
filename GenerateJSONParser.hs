{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE ViewPatterns         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Applicative
import           Control.Monad             (forM_, when, unless)
import           Data.Maybe
import           Data.List                 (partition)
import           System.Exit
import           System.IO                 (stdin, stderr, stdout, IOMode(..))
import           System.FilePath           (splitExtension)
import           System.Process            (system)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict        as Map
import           Data.Aeson
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Data.Text                 (Text)
import           Text.PrettyPrint.GenericPretty (pretty)

import           Data.Aeson.AutoType.Pretty
import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.CodeGen
import           Data.Aeson.AutoType.Util
import           HFlags

-- * Command line flags
defineFlag "o:outputFilename"  defaultOutputFilename "Write output to the given file"
defineFlag "suggest"           True                  "Suggest candidates for unification"
defineFlag "autounify"         True                  "Automatically unify suggested candidates"
defineFlag "t:test"            False                 "Try to run generated parser after"
defineFlag "d:debug"           False                 "Set this flag to see more debugging info"
defineFlag "y:typecheck"       True                  "Set this flag to typecheck after unification"
defineFlag "p:preprocessor"    False                 "Work as GHC preprocessor (skip preprocessor pragma)"
defineFlag "fakeFlag"          True                  "Ignore this flag - it doesn't exist!!! It is workaround to library problem."

-- | Works like @Debug.trace@ when the --debug flag is enabled, and does nothing otherwise.
myTrace :: String -> IO ()
myTrace msg = flags_debug `when` putStrLn msg

-- | Report an error to error output.
report   :: Text -> IO ()
report    = Text.hPutStrLn stderr

-- | Report an error and terminate the program.
fatal    :: Text -> IO ()
fatal msg = do report msg
               exitFailure

-- | Extracts type from JSON file, along with the original @Value@.
-- In order to facilitate dealing with failures, it returns a triple of
-- @FilePath@, extracted @Type@, and a JSON @Value@.
extractTypeFromJSONFile :: FilePath -> IO (Maybe (FilePath, Type, Value))
extractTypeFromJSONFile inputFilename =
      withFileOrHandle inputFilename ReadMode stdin $ \hIn ->
        -- First we decode JSON input into Aeson's Value type
        do bs <- preprocess <$> BSL.hGetContents hIn
           Text.hPutStrLn stderr $ "Processing " `Text.append` Text.pack (show inputFilename)
           myTrace ("Decoded JSON: " ++ pretty (decode bs :: Maybe Value))
           case decode bs of
             Nothing -> do report $ "Cannot decode JSON input from " `Text.append` Text.pack (show inputFilename)
                           return Nothing
             Just v  -> do -- If decoding JSON was successful...
               -- We extract type structure from the JSON value.
               let t :: Type = extractType v
               myTrace $ "Type: " ++ pretty t
               (v `typeCheck` t) `unless` fatal ("Typecheck against base type failed for "
                                                    `Text.append` Text.pack inputFilename)
               return $ Just (inputFilename, t, v)

-- | Perform preprocessing of JSON input to drop initial pragma.
preprocess :: BSL.ByteString -> BSL.ByteString
preprocess | flags_preprocessor = dropPragma
           | otherwise          = id

-- | Drop initial pragma.
dropPragma :: BSL.ByteString -> BSL.ByteString
dropPragma input | "{-#" `BSL.isPrefixOf` input = BSL.dropWhile (/='\n') input
                 | otherwise                    = input

-- | Type checking all input files with given type,
-- and return a list of filenames for files that passed the check.
typeChecking :: Type -> [FilePath] -> [Value] -> IO [FilePath]
typeChecking ty filenames values = do
    unless (null failures) $ report $ Text.unwords $ "Failed to typecheck with unified type: ":
                                                          (Text.pack `map` failures)
    when (      null successes) $ fatal    "No files passed the typecheck."
    return successes
  where
    checkedFiles = zip filenames $ map (`typeCheck` ty) values
    (map fst -> successes,
     map fst -> failures) = partition snd checkedFiles

-- | Take a set of JSON input filenames, Haskell output filename, and generate module parsing these JSON files.
generateHaskellFromJSONs :: [FilePath] -> FilePath -> IO ()
generateHaskellFromJSONs inputFilenames outputFilename = do
  -- Read type from each file
  (filenames,
   typeForEachFile,
   valueForEachFile) <- (unzip3 . catMaybes) <$> mapM extractTypeFromJSONFile inputFilenames
  -- Unify all input types
  when (null typeForEachFile) $ do
    report "No valid JSON input file..."
    exitFailure
  let finalType = foldr1 unifyTypes typeForEachFile
  passedTypeCheck <- if flags_typecheck
                        then typeChecking finalType filenames valueForEachFile
                        else return                 filenames
  -- We split different dictionary labels to become different type trees (and thus different declarations.)
  let splitted = splitTypeByLabel "TopLevel" finalType
  myTrace $ "SPLITTED: " ++ pretty splitted
  assertM $ not $ any hasNonTopTObj $ Map.elems splitted
  -- We compute which type labels are candidates for unification
  let uCands = unificationCandidates splitted
  myTrace $ "CANDIDATES:\n" ++ pretty uCands
  when flags_suggest $ forM_ uCands $ \cs -> do
                         putStr "-- "
                         Text.putStrLn $ "=" `Text.intercalate` cs
  -- We unify the all candidates or only those that have been given as command-line flags.
  let unified = if flags_autounify
                  then unifyCandidates uCands splitted
                  else splitted
  myTrace $ "UNIFIED:\n" ++ pretty unified
  -- We start by writing module header
  writeHaskellModule outputFilename unified
  when flags_test $
    exitWith =<< system (unwords $ ["runghc", outputFilename] ++ passedTypeCheck)

-- | Initialize flags, and run @generateHaskellFromJSONs@.
main :: IO ()
main = do filenames <- $initHFlags "json-autotype -- automatic type and parser generation from JSON"
          -- TODO: should integrate all inputs into single type set!!!
          generateHaskellFromJSONs filenames flags_outputFilename
