
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesJsonSchemaFstab where

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

data Storage = Storage { 
    storageDevice :: (Maybe (Text:|:[(Maybe Value)])),
    storageRemotePath :: (Maybe (Text:|:[(Maybe Value)])),
    storageServer :: (Maybe (Text:|:[(Maybe Value)])),
    storageSizeInMB :: (Maybe (Int:|:[(Maybe Value)])),
    storageType :: Text,
    storageLabel :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Storage where
  parseJSON (Object v) = Storage <$> v .:? "device" <*> v .:? "remotePath" <*> v .:? "server" <*> v .:? "sizeInMB" <*> v .:  "type" <*> v .:? "label"
  parseJSON _          = mzero


instance ToJSON Storage where
  toJSON     (Storage {..}) = object ["device" .= storageDevice, "remotePath" .= storageRemotePath, "server" .= storageServer, "sizeInMB" .= storageSizeInMB, "type" .= storageType, "label" .= storageLabel]
  toEncoding (Storage {..}) = pairs  ("device" .= storageDevice<>"remotePath" .= storageRemotePath<>"server" .= storageServer<>"sizeInMB" .= storageSizeInMB<>"type" .= storageType<>"label" .= storageLabel)


data JsonEmptyKey = JsonEmptyKey { 
    jsonEmptyKeyFstype :: Text,
    jsonEmptyKeyStorage :: Storage,
    jsonEmptyKeyReadonly :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON JsonEmptyKey where
  parseJSON (Object v) = JsonEmptyKey <$> v .:  "fstype" <*> v .:  "storage" <*> v .:  "readonly"
  parseJSON _          = mzero


instance ToJSON JsonEmptyKey where
  toJSON     (JsonEmptyKey {..}) = object ["fstype" .= jsonEmptyKeyFstype, "storage" .= jsonEmptyKeyStorage, "readonly" .= jsonEmptyKeyReadonly]
  toEncoding (JsonEmptyKey {..}) = pairs  ("fstype" .= jsonEmptyKeyFstype<>"storage" .= jsonEmptyKeyStorage<>"readonly" .= jsonEmptyKeyReadonly)


data Tmp = Tmp { 
    tmpStorage :: Storage:|:[(Maybe Value)]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Tmp where
  parseJSON (Object v) = Tmp <$> v .:  "storage"
  parseJSON _          = mzero


instance ToJSON Tmp where
  toJSON     (Tmp {..}) = object ["storage" .= tmpStorage]
  toEncoding (Tmp {..}) = pairs  ("storage" .= tmpStorage)


data Var = Var { 
    varFstype :: Text,
    varStorage :: Storage,
    varOptions :: [Text]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Var where
  parseJSON (Object v) = Var <$> v .:  "fstype" <*> v .:  "storage" <*> v .:  "options"
  parseJSON _          = mzero


instance ToJSON Var where
  toJSON     (Var {..}) = object ["fstype" .= varFstype, "storage" .= varStorage, "options" .= varOptions]
  toEncoding (Var {..}) = pairs  ("fstype" .= varFstype<>"storage" .= varStorage<>"options" .= varOptions)


data TopLevel = TopLevel { 
    topLevelVar :: Var,
    topLevelJsonEmptyKey :: JsonEmptyKey,
    topLevelVarWww :: Tmp,
    topLevelTmp :: Tmp
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "/var" <*> v .:  "/" <*> v .:  "/var/www" <*> v .:  "/tmp"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["/var" .= topLevelVar, "/" .= topLevelJsonEmptyKey, "/var/www" .= topLevelVarWww, "/tmp" .= topLevelTmp]
  toEncoding (TopLevel {..}) = pairs  ("/var" .= topLevelVar<>"/" .= topLevelJsonEmptyKey<>"/var/www" .= topLevelVarWww<>"/tmp" .= topLevelTmp)




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

