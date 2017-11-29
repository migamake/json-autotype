{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Applicative
import           Control.Monad.State        as State
import           Data.Maybe
import           System.Exit
import           System.IO                 (stdin, stderr, stdout, IOMode(..))
import           System.FilePath           (splitExtension, (<.>))
import           System.Directory          (removeFile)
import           System.Process            (system)
import           Control.Monad             (forM_, forM, when)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict        as Map
import qualified Data.Set                   as Set
import           Data.Monoid               ((<>))
import           Data.Aeson                (Value(..), decode, encode, FromJSON(..), ToJSON(..))
import           Data.Function             (on)
import           Data.List
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Data.Text                 (Text)
import qualified Data.Vector                as V
import           Data.Scientific           (scientific, Scientific)
import           Text.PrettyPrint.GenericPretty (pretty)
import           Test.QuickCheck

import           Data.Aeson.AutoType.Pretty
import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.CodeGen
import           Data.Aeson.AutoType.Util
import           Data.Aeson.AutoType.Test
import           Options.Applicative

import           CommonCLI

-- * Command line flags
--defineFlag "z:size"            (10     :: Int)        "Size of generated elements"
--defineFlag "s:stem"            ("Test" :: FilePath)   "Test filename stem"
--defineFlag "c:count"           (100    :: Int)        "Number of test cases to generate."
--defineFlag "o:outputFilename"   defaultOutputFilename "Write output to the given file"
--flags_suggest = True
--defineFlag "suggest"            True                  "Suggest candidates for unification"
--defineFlag "autounify"          True                  "Automatically unify suggested candidates"
--defineFlag "t:test"             True                  "Try to run generated parser after"
--defineFlag "d:debug"            False                 "Set this flag to see more debugging info"
--defineFlag "keep"               False                 "Keep also the successful tests"
--defineFlag "fakeFlag"           True                  "Ignore this flag - it doesn't exist!!! It is workaround for a library problem."

data Options = Options {
                 tyOpts    :: TypeOpts
               , keep      :: Bool
               , stem      :: FilePath
               , count     :: Int
               , size      :: Int
               }

optParser :: Parser Options
optParser  =
    Options  <$> tyOptParser
             <*> switch    (long "keep"                  <> help "Also keep successful tests"  )
             <*> strOption (long "stem"  <> value "Test" <> help "Output filename stem"        )
             <*> intOpt    (long "count" <> value 100    <> help "Number of tests to perform"  )
             <*> intOpt    (long "size"  <> value 10     <> help "size of generated test cases")
             -- <*> some (argument str (metavar "FILES..."))
  where
    intOpt = option auto

-- | Report an error to error output.
report   :: Text -> IO ()
report    = Text.hPutStrLn stderr

-- | Report an error and terminate the program.
fatal    :: Text -> IO ()
fatal msg = do report msg
               exitFailure

-- | Read JSON and extract @Type@ information from it.
extractTypeFromJSONFile :: (String -> IO ()) -> FilePath -> IO (Maybe Type)
extractTypeFromJSONFile myTrace inputFilename =
      withFileOrHandle inputFilename ReadMode stdin $ \hIn ->
        -- First we decode JSON input into Aeson's Value type
        do bs <- BSL.hGetContents hIn
           Text.hPutStrLn stderr $ "Processing " `Text.append` Text.pack (show inputFilename)
           myTrace ("Decoded JSON: " ++ pretty (decode bs :: Maybe Value))
           case decode bs of
             Nothing -> do report $ "Cannot decode JSON input from " `Text.append` Text.pack (show inputFilename)
                           return Nothing
             Just v  -> do -- If decoding JSON was successful...
               -- We extract type structure from the JSON value.
               let t        = extractType v
               myTrace $ "Type: " ++ pretty t
               return $ Just t


vectorWithoutDuplicates ::  Ord b => Int -> Gen b -> Gen [b]
vectorWithoutDuplicates i gen = take i
                              .  removeDuplicates
                             <$> infiniteListOf gen

removeDuplicates ::  Ord a => [a] -> [a]
removeDuplicates list = filterM checkDup list `evalState` Set.empty
  where
    checkDup x = do seen <- State.get
                    if x `Set.member` seen
                      then
                        return False
                      else do
                        State.put $ x `Set.insert` seen
                        return True

-- TODO: check for generic Ord?
instance Ord Value where
  Null       `compare`  Null      = EQ
  Null       `compare`  _         = LT
  _          `compare`  Null      = GT
  (Bool   a) `compare` (Bool   b) = a `compare` b
  (Bool   a) `compare`  _         = LT
  _          `compare` (Bool   b) = GT
  (Number a) `compare` (Number b) = a `compare` b
  (Number _) `compare`  _         = LT
  _          `compare` (Number _) = GT
  (String a) `compare` (String b) = a `compare` b
  (String a) `compare` _          = LT
  _          `compare` (String b) = GT
  (Array  a) `compare` (Array  b) = a `compare` b
  (Array  a) `compare` _          = LT
  _          `compare` (Array  b) = GT
  (Object a) `compare` (Object b) = Map.toList a `compare` Map.toList b

-- | Take a set of JSON input filenames, Haskell output filename, and generate module parsing these JSON files.
generateTestJSONs :: Options -> IO ()
generateTestJSONs Options {tyOpts=TyOptions {..},
                           toplevel,
                           ..}= do
    testValues :: [Value] <- generate $
                               resize size $
                                 vectorWithoutDuplicates 100 arbitraryTopValue
    results               <- forM (zip3 inputFilenames outputFilenames testValues) $
      \(inputFilename, outputFilename, jsonValue) -> do
        BSL.writeFile inputFilename $ encode jsonValue
        -- Read type from each file
        typeForEachFile  <- catMaybes <$> mapM (extractTypeFromJSONFile myTrace) [inputFilename]
        -- Unify all input types
        when (null typeForEachFile) $ do
          report "No valid JSON input file..."
          exitFailure
        let finalType = foldr1 unifyTypes typeForEachFile
        -- We split different dictionary labels to become different type trees (and thus different declarations.)
        let splitted = splitTypeByLabel toplevel finalType
        --myTrace $ "SPLITTED: " ++ pretty splitted
        assertM $ not $ any hasNonTopTObj $ Map.elems splitted
        -- We compute which type labels are candidates for unification
        let uCands = unificationCandidates splitted
        myTrace $ "CANDIDATES:\n" ++ pretty uCands
        when suggest $ forM_ uCands $ \cs -> do
                               putStr "-- "
                               Text.putStrLn $ "=" `Text.intercalate` cs
        -- We unify the all candidates or only those that have been given as command-line flags.
        let unified = if autounify
                        then unifyCandidates uCands splitted
                        else splitted
        myTrace $ "UNIFIED:\n" ++ pretty unified
        -- We start by writing module header
        writeHaskellModule outputFilename unified
        if test
          then do
            r <- (==ExitSuccess) <$> runghc [outputFilename, inputFilename]
            when r $ mapM_ removeFile [inputFilename, outputFilename]
            return r
          else
            return True
    putStrLn $ "Successfully generated "      ++ show (length results) ++
               " JSON files, out of planned " ++ show count  ++ " cases."
  where
    makeInputFilename  = (<.>".json") . (stem ++) . show
    makeOutputFilename = (<.>".hs")   . (stem ++) . show
    inputFilenames     = map makeInputFilename  [1..count]
    outputFilenames    = map makeOutputFilename [1..count]
    myTrace :: String -> IO ()
    myTrace msg = debug `when` putStrLn msg
    toplevelName = capitalize $ Text.pack toplevel

main :: IO ()
main = do opts <- execParser optInfo
          generateTestJSONs opts
    where
      optInfo = info (optParser <**> helper)
        ( fullDesc
       <> progDesc "Generate a number of JSON test files, and generate type and parser for each."
       <> header   "Self-test for json-autotype" )
