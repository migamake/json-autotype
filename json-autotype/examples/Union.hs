
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesUnion where

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

data ParameterElt = ParameterElt { 
    parameterEltParameterValue :: Bool:|:Text:|:Int:|:[(Maybe Value)],
    parameterEltParameterName :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ParameterElt where
  parseJSON (Object v) = ParameterElt <$> v .:  "parameterValue" <*> v .:  "parameterName"
  parseJSON _          = mzero


instance ToJSON ParameterElt where
  toJSON     (ParameterElt {..}) = object ["parameterValue" .= parameterEltParameterValue, "parameterName" .= parameterEltParameterName]
  toEncoding (ParameterElt {..}) = pairs  ("parameterValue" .= parameterEltParameterValue<>"parameterName" .= parameterEltParameterName)


data TopLevel = TopLevel { 
    topLevelParameter :: [ParameterElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "parameter"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["parameter" .= topLevelParameter]
  toEncoding (TopLevel {..}) = pairs  ("parameter" .= topLevelParameter)




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

