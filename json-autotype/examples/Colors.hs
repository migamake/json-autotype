
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesColors where

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

data ColorsArrayElt = ColorsArrayElt { 
    colorsArrayEltHexValue :: Text,
    colorsArrayEltColorName :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON ColorsArrayElt where
  parseJSON (Object v) = ColorsArrayElt <$> v .:  "hexValue" <*> v .:  "colorName"
  parseJSON _          = mzero


instance ToJSON ColorsArrayElt where
  toJSON     (ColorsArrayElt {..}) = object ["hexValue" .= colorsArrayEltHexValue, "colorName" .= colorsArrayEltColorName]
  toEncoding (ColorsArrayElt {..}) = pairs  ("hexValue" .= colorsArrayEltHexValue<>"colorName" .= colorsArrayEltColorName)


data TopLevel = TopLevel { 
    topLevelColorsArray :: [ColorsArrayElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "colorsArray"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["colorsArray" .= topLevelColorsArray]
  toEncoding (TopLevel {..}) = pairs  ("colorsArray" .= topLevelColorsArray)




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

