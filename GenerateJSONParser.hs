{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           System.IO                 (stdin, stderr, stdout, IOMode(..))
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
                      ,"import           Data.Text (Text)"
                      ,"import           Data.Aeson(decode, Value(..), FromJSON(..),"
                      ,"                            (.:), (.:?), (.!=))"
                      ,"import           Data.Aeson.TH"
                      ,""]

-- * Command line flags
defineFlag "filename"  (defaultOutputFilename :: FilePath) "Write output to the given file"
defineFlag "suggest"   True                                "Suggest candidates for unification"
defineFlag "autounify" True                                "Automatically unify suggested candidates"
defineFlag "fakeFlag"  True                                "Ignore this flag - it doesn't exist!!!"

-- Tracing is switched off:
myTrace :: String -> IO ()
myTrace _msg = return ()
--myTrace = putStrLn 

main :: IO ()
main = do filenames <- $initHFlags "json-autotype -- automatic type and parser generation from JSON"
          let (moduleName, extension) = splitExtension $
                                          if flags_filename == "-"
                                            then defaultOutputFilename
                                            else flags_filename
          assertM (extension == ".hs")
          -- TODO: should integrate all inputs into single type set!!!
          withFileOrHandle flags_filename WriteMode stdout $ \hOut ->
            forM_ filenames $ \filename ->
              withFileOrHandle filename ReadMode stdin $ \hIn ->
                do bs <- BSL.hGetContents hIn
                   Text.hPutStrLn stderr $ "Processing " `Text.append` Text.pack (show moduleName)
                   myTrace ("Decoded JSON: " ++ show (decode bs :: Maybe Value))
                   let Just v   = decode bs
                   let t        = extractType v
                   myTrace $ "type: " ++ show t
                   let splitted = splitTypeByLabel "TopLevel" t
                   myTrace $ "splitted: " ++ show splitted
                   Text.hPutStrLn hOut $ header $ Text.pack moduleName
                   assertM $ not $ any hasNonTopTObj $ Map.elems splitted
                   let uCands = unificationCandidates splitted
                   myTrace $ "candidates: " ++ show uCands
                   when flags_suggest $ forM_ uCands $ \cs -> do
                                          putStr "-- "
                                          Text.putStrLn $ "=" `Text.intercalate` cs
                   let unified = if flags_autounify
                                   then unifyCandidates uCands splitted
                                   else splitted
                   myTrace $ "unified: " ++ show unified
                   Text.hPutStrLn hOut $ displaySplitTypes unified

