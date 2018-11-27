
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesTranscripticProtocolPcr where

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

data StepsElt = StepsElt { 
    stepsEltTemperature :: Text,
    stepsEltDuration :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON StepsElt where
  parseJSON (Object v) = StepsElt <$> v .:  "temperature" <*> v .:  "duration"
  parseJSON _          = mzero


instance ToJSON StepsElt where
  toJSON     (StepsElt {..}) = object ["temperature" .= stepsEltTemperature, "duration" .= stepsEltDuration]
  toEncoding (StepsElt {..}) = pairs  ("temperature" .= stepsEltTemperature<>"duration" .= stepsEltDuration)


data TransferElt = TransferElt { 
    transferEltVolume :: Text,
    transferEltTo :: Text,
    transferEltFrom :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TransferElt where
  parseJSON (Object v) = TransferElt <$> v .:  "volume" <*> v .:  "to" <*> v .:  "from"
  parseJSON _          = mzero


instance ToJSON TransferElt where
  toJSON     (TransferElt {..}) = object ["volume" .= transferEltVolume, "to" .= transferEltTo, "from" .= transferEltFrom]
  toEncoding (TransferElt {..}) = pairs  ("volume" .= transferEltVolume<>"to" .= transferEltTo<>"from" .= transferEltFrom)


data GroupsElt = GroupsElt { 
    groupsEltCycles :: (Maybe (Int:|:[(Maybe Value)])),
    groupsEltSteps :: (Maybe ([StepsElt])),
    groupsEltTransfer :: (Maybe ([TransferElt]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON GroupsElt where
  parseJSON (Object v) = GroupsElt <$> v .:? "cycles" <*> v .:? "steps" <*> v .:? "transfer"
  parseJSON _          = mzero


instance ToJSON GroupsElt where
  toJSON     (GroupsElt {..}) = object ["cycles" .= groupsEltCycles, "steps" .= groupsEltSteps, "transfer" .= groupsEltTransfer]
  toEncoding (GroupsElt {..}) = pairs  ("cycles" .= groupsEltCycles<>"steps" .= groupsEltSteps<>"transfer" .= groupsEltTransfer)


data InstructionsElt = InstructionsElt { 
    instructionsEltGroups :: (Maybe ([GroupsElt])),
    instructionsEltOp :: Text,
    instructionsEltObject :: (Maybe (Text:|:[(Maybe Value)])),
    instructionsEltLid :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON InstructionsElt where
  parseJSON (Object v) = InstructionsElt <$> v .:? "groups" <*> v .:  "op" <*> v .:? "object" <*> v .:? "lid"
  parseJSON _          = mzero


instance ToJSON InstructionsElt where
  toJSON     (InstructionsElt {..}) = object ["groups" .= instructionsEltGroups, "op" .= instructionsEltOp, "object" .= instructionsEltObject, "lid" .= instructionsEltLid]
  toEncoding (InstructionsElt {..}) = pairs  ("groups" .= instructionsEltGroups<>"op" .= instructionsEltOp<>"object" .= instructionsEltObject<>"lid" .= instructionsEltLid)


data Store = Store { 
    storeWhere :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Store where
  parseJSON (Object v) = Store <$> v .:  "where"
  parseJSON _          = mzero


instance ToJSON Store where
  toJSON     (Store {..}) = object ["where" .= storeWhere]
  toEncoding (Store {..}) = pairs  ("where" .= storeWhere)


data PcrPlate = PcrPlate { 
    pcrPlateNew :: Text,
    pcrPlateStore :: Store
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON PcrPlate where
  parseJSON (Object v) = PcrPlate <$> v .:  "new" <*> v .:  "store"
  parseJSON _          = mzero


instance ToJSON PcrPlate where
  toJSON     (PcrPlate {..}) = object ["new" .= pcrPlateNew, "store" .= pcrPlateStore]
  toEncoding (PcrPlate {..}) = pairs  ("new" .= pcrPlateNew<>"store" .= pcrPlateStore)


data InputPlate = InputPlate { 
    inputPlateStore :: Store,
    inputPlateId :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON InputPlate where
  parseJSON (Object v) = InputPlate <$> v .:  "store" <*> v .:  "id"
  parseJSON _          = mzero


instance ToJSON InputPlate where
  toJSON     (InputPlate {..}) = object ["store" .= inputPlateStore, "id" .= inputPlateId]
  toEncoding (InputPlate {..}) = pairs  ("store" .= inputPlateStore<>"id" .= inputPlateId)


data Refs = Refs { 
    refsPcrPlate :: PcrPlate,
    refsInputPlate :: InputPlate
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Refs where
  parseJSON (Object v) = Refs <$> v .:  "pcr_plate" <*> v .:  "input_plate"
  parseJSON _          = mzero


instance ToJSON Refs where
  toJSON     (Refs {..}) = object ["pcr_plate" .= refsPcrPlate, "input_plate" .= refsInputPlate]
  toEncoding (Refs {..}) = pairs  ("pcr_plate" .= refsPcrPlate<>"input_plate" .= refsInputPlate)


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

