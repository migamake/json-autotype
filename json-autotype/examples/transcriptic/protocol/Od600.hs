
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesTranscripticProtocolOd600 where

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

data InstructionsElt = InstructionsElt { 
    instructionsEltOp :: Text,
    instructionsEltWhere :: (Maybe (Text:|:[(Maybe Value)])),
    instructionsEltWavelength :: (Maybe (Text:|:[(Maybe Value)])),
    instructionsEltObject :: Text,
    instructionsEltDuration :: (Maybe (Text:|:[(Maybe Value)])),
    instructionsEltDataref :: (Maybe (Text:|:[(Maybe Value)])),
    instructionsEltWells :: (Maybe ([Text])),
    instructionsEltShaking :: (Maybe (Bool:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON InstructionsElt where
  parseJSON (Object v) = InstructionsElt <$> v .:  "op" <*> v .:? "where" <*> v .:? "wavelength" <*> v .:  "object" <*> v .:? "duration" <*> v .:? "dataref" <*> v .:? "wells" <*> v .:? "shaking"
  parseJSON _          = mzero


instance ToJSON InstructionsElt where
  toJSON     (InstructionsElt {..}) = object ["op" .= instructionsEltOp, "where" .= instructionsEltWhere, "wavelength" .= instructionsEltWavelength, "object" .= instructionsEltObject, "duration" .= instructionsEltDuration, "dataref" .= instructionsEltDataref, "wells" .= instructionsEltWells, "shaking" .= instructionsEltShaking]
  toEncoding (InstructionsElt {..}) = pairs  ("op" .= instructionsEltOp<>"where" .= instructionsEltWhere<>"wavelength" .= instructionsEltWavelength<>"object" .= instructionsEltObject<>"duration" .= instructionsEltDuration<>"dataref" .= instructionsEltDataref<>"wells" .= instructionsEltWells<>"shaking" .= instructionsEltShaking)


data Store = Store { 
    storeWhere :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Store where
  parseJSON (Object v) = Store <$> v .:  "where"
  parseJSON _          = mzero


instance ToJSON Store where
  toJSON     (Store {..}) = object ["where" .= storeWhere]
  toEncoding (Store {..}) = pairs  ("where" .= storeWhere)


data MyPlate = MyPlate { 
    myPlateStore :: Store,
    myPlateId :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON MyPlate where
  parseJSON (Object v) = MyPlate <$> v .:  "store" <*> v .:  "id"
  parseJSON _          = mzero


instance ToJSON MyPlate where
  toJSON     (MyPlate {..}) = object ["store" .= myPlateStore, "id" .= myPlateId]
  toEncoding (MyPlate {..}) = pairs  ("store" .= myPlateStore<>"id" .= myPlateId)


data Refs = Refs { 
    refsMyPlate :: MyPlate
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Refs where
  parseJSON (Object v) = Refs <$> v .:  "my_plate"
  parseJSON _          = mzero


instance ToJSON Refs where
  toJSON     (Refs {..}) = object ["my_plate" .= refsMyPlate]
  toEncoding (Refs {..}) = pairs  ("my_plate" .= refsMyPlate)


data TopLevel = TopLevel { 
    topLevelInstructions :: [InstructionsElt],
    topLevelRefs :: Refs
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "instructions" <*> v .:  "refs"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["instructions" .= topLevelInstructions, "refs" .= topLevelRefs]
  toEncoding (TopLevel {..}) = pairs  ("instructions" .= topLevelInstructions<>"refs" .= topLevelRefs)




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

