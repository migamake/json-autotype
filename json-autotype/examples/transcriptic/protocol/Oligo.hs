
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesTranscripticProtocolOligo where

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

data Sequence = Sequence { 
    sequenceSequence :: Text,
    sequenceName :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Sequence where
  parseJSON (Object v) = Sequence <$> v .:  "sequence" <*> v .:  "name"
  parseJSON _          = mzero


instance ToJSON Sequence where
  toJSON     (Sequence {..}) = object ["sequence" .= sequenceSequence, "name" .= sequenceName]
  toEncoding (Sequence {..}) = pairs  ("sequence" .= sequenceSequence<>"name" .= sequenceName)


data TopLevel = TopLevel { 
    topLevelScale :: Text,
    topLevelSequence :: Sequence,
    topLevelType :: Text,
    topLevelPurity :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "scale" <*> v .:  "sequence" <*> v .:  "type" <*> v .:  "purity"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["scale" .= topLevelScale, "sequence" .= topLevelSequence, "type" .= topLevelType, "purity" .= topLevelPurity]
  toEncoding (TopLevel {..}) = pairs  ("scale" .= topLevelScale<>"sequence" .= topLevelSequence<>"type" .= topLevelType<>"purity" .= topLevelPurity)




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

