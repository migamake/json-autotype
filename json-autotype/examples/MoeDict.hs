
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesMoeDict where

import           System.Exit        (exitFailure, exitSuccess)
import           System.IO          (stderr, hPutStrLn)
import qualified Data.ByteString.Lazy.Char8 as BSL
import           System.Environment (getArgs)
import           Control.Monad      (forM_, mzero, join)
import           Control.Applicative
import           Data.Aeson.AutoType.Alternative
import           Data.Aeson(eitherDecode, Value(..), FromJSON(..), ToJSON(..),
                            pairs,
                            (.:), (.:?), (.=), object)
import           Data.Monoid((<>))
import           Data.Text (Text)
import qualified GHC.Generics

data DefinitionsElt = DefinitionsElt { 
    definitionsEltAntonyms :: (Maybe (Text:|:[(Maybe Value)])),
    definitionsEltLink :: (Maybe ([Text])),
    definitionsEltExample :: (Maybe ([Text])),
    definitionsEltDef :: Text,
    definitionsEltQuote :: (Maybe ([Text])),
    definitionsEltSynonyms :: (Maybe (Text:|:[(Maybe Value)])),
    definitionsEltType :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON DefinitionsElt where
  parseJSON (Object v) = DefinitionsElt <$> v .:? "antonyms" <*> v .:? "link" <*> v .:? "example" <*> v .:  "def" <*> v .:? "quote" <*> v .:? "synonyms" <*> v .:? "type"
  parseJSON _          = mzero


instance ToJSON DefinitionsElt where
  toJSON     (DefinitionsElt {..}) = object ["antonyms" .= definitionsEltAntonyms, "link" .= definitionsEltLink, "example" .= definitionsEltExample, "def" .= definitionsEltDef, "quote" .= definitionsEltQuote, "synonyms" .= definitionsEltSynonyms, "type" .= definitionsEltType]
  toEncoding (DefinitionsElt {..}) = pairs  ("antonyms" .= definitionsEltAntonyms<>"link" .= definitionsEltLink<>"example" .= definitionsEltExample<>"def" .= definitionsEltDef<>"quote" .= definitionsEltQuote<>"synonyms" .= definitionsEltSynonyms<>"type" .= definitionsEltType)


data HeteronymsElt = HeteronymsElt { 
    heteronymsEltBopomofo2 :: Text,
    heteronymsEltBopomofo :: Text,
    heteronymsEltPinyin :: Text,
    heteronymsEltDefinitions :: [DefinitionsElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON HeteronymsElt where
  parseJSON (Object v) = HeteronymsElt <$> v .:  "bopomofo2" <*> v .:  "bopomofo" <*> v .:  "pinyin" <*> v .:  "definitions"
  parseJSON _          = mzero


instance ToJSON HeteronymsElt where
  toJSON     (HeteronymsElt {..}) = object ["bopomofo2" .= heteronymsEltBopomofo2, "bopomofo" .= heteronymsEltBopomofo, "pinyin" .= heteronymsEltPinyin, "definitions" .= heteronymsEltDefinitions]
  toEncoding (HeteronymsElt {..}) = pairs  ("bopomofo2" .= heteronymsEltBopomofo2<>"bopomofo" .= heteronymsEltBopomofo<>"pinyin" .= heteronymsEltPinyin<>"definitions" .= heteronymsEltDefinitions)


data TopLevelElt = TopLevelElt { 
    topLevelEltHeteronyms :: [HeteronymsElt],
    topLevelEltNonRadicalStrokeCount :: (Maybe (Int:|:[(Maybe Value)])),
    topLevelEltRadical :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltDeutsch :: (Maybe (Text:|:[(Maybe Value)])),
    topLevelEltTitle :: Text,
    topLevelEltStrokeCount :: (Maybe (Int:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevelElt where
  parseJSON (Object v) = TopLevelElt <$> v .:  "heteronyms" <*> v .:? "non_radical_stroke_count" <*> v .:? "radical" <*> v .:? "Deutsch" <*> v .:  "title" <*> v .:? "stroke_count"
  parseJSON _          = mzero


instance ToJSON TopLevelElt where
  toJSON     (TopLevelElt {..}) = object ["heteronyms" .= topLevelEltHeteronyms, "non_radical_stroke_count" .= topLevelEltNonRadicalStrokeCount, "radical" .= topLevelEltRadical, "Deutsch" .= topLevelEltDeutsch, "title" .= topLevelEltTitle, "stroke_count" .= topLevelEltStrokeCount]
  toEncoding (TopLevelElt {..}) = pairs  ("heteronyms" .= topLevelEltHeteronyms<>"non_radical_stroke_count" .= topLevelEltNonRadicalStrokeCount<>"radical" .= topLevelEltRadical<>"Deutsch" .= topLevelEltDeutsch<>"title" .= topLevelEltTitle<>"stroke_count" .= topLevelEltStrokeCount)


type TopLevel = [TopLevelElt]



parse :: FilePath -> IO TopLevel
parse filename = do
    input <- BSL.readFile filename
    case eitherDecode input of
      Left  err -> fatal $ case (eitherDecode input :: Either String Value) of
                           Left  err -> "Invalid JSON file: " ++ filename ++ " ++ err"
                           Right _   -> "Mismatched JSON value from file: " ++ filename
                                     ++ "\n" ++ err
      Right r   -> return (r :: TopLevel)
  where
    fatal :: String -> IO a
    fatal msg = do hPutStrLn stderr msg
                   exitFailure

main :: IO ()
main = do
  filenames <- getArgs
  forM_ filenames (\f -> parse f >>= (\p -> p `seq` putStrLn $ "Successfully parsed " ++ f))
  exitSuccess

