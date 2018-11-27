
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesTranscripticInstructionInstructions where

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
    groupsEltTransfer :: [TransferElt]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON GroupsElt where
  parseJSON (Object v) = GroupsElt <$> v .:  "transfer"
  parseJSON _          = mzero


instance ToJSON GroupsElt where
  toJSON     (GroupsElt {..}) = object ["transfer" .= groupsEltTransfer]
  toEncoding (GroupsElt {..}) = pairs  ("transfer" .= groupsEltTransfer)


data InstructionsElt = InstructionsElt { 
    instructionsEltGroups :: (Maybe ([GroupsElt])),
    instructionsEltOp :: Text,
    instructionsEltSpeed :: (Maybe (Text:|:[(Maybe Value)])),
    instructionsEltObject :: (Maybe (Text:|:[(Maybe Value)])),
    instructionsEltDuration :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON InstructionsElt where
  parseJSON (Object v) = InstructionsElt <$> v .:? "groups" <*> v .:  "op" <*> v .:? "speed" <*> v .:? "object" <*> v .:? "duration"
  parseJSON _          = mzero


instance ToJSON InstructionsElt where
  toJSON     (InstructionsElt {..}) = object ["groups" .= instructionsEltGroups, "op" .= instructionsEltOp, "speed" .= instructionsEltSpeed, "object" .= instructionsEltObject, "duration" .= instructionsEltDuration]
  toEncoding (InstructionsElt {..}) = pairs  ("groups" .= instructionsEltGroups<>"op" .= instructionsEltOp<>"speed" .= instructionsEltSpeed<>"object" .= instructionsEltObject<>"duration" .= instructionsEltDuration)


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

