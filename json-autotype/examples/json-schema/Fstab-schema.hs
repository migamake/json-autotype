
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesJsonSchemaFstabSchema where

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

data Type = Type { 
    typeEnum :: [Text]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Type where
  parseJSON (Object v) = Type <$> v .:  "enum"
  parseJSON _          = mzero


instance ToJSON Type where
  toJSON     (Type {..}) = object ["enum" .= typeEnum]
  toEncoding (Type {..}) = pairs  ("enum" .= typeEnum)


data Label = Label { 
    labelPattern :: Text,
    labelType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Label where
  parseJSON (Object v) = Label <$> v .:  "pattern" <*> v .:  "type"
  parseJSON _          = mzero


instance ToJSON Label where
  toJSON     (Label {..}) = object ["pattern" .= labelPattern, "type" .= labelType]
  toEncoding (Label {..}) = pairs  ("pattern" .= labelPattern<>"type" .= labelType)


data OneOfElt = OneOfElt { 
    oneOfEltRef :: (Maybe (Text:|:[(Maybe Value)])),
    oneOfEltFormat :: (Maybe (Text:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON OneOfElt where
  parseJSON (Object v) = OneOfElt <$> v .:? "$ref" <*> v .:? "format"
  parseJSON _          = mzero


instance ToJSON OneOfElt where
  toJSON     (OneOfElt {..}) = object ["$ref" .= oneOfEltRef, "format" .= oneOfEltFormat]
  toEncoding (OneOfElt {..}) = pairs  ("$ref" .= oneOfEltRef<>"format" .= oneOfEltFormat)


data Server = Server { 
    serverOneOf :: [OneOfElt:|:[(Maybe Value)]],
    serverType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Server where
  parseJSON (Object v) = Server <$> v .:  "oneOf" <*> v .:  "type"
  parseJSON _          = mzero


instance ToJSON Server where
  toJSON     (Server {..}) = object ["oneOf" .= serverOneOf, "type" .= serverType]
  toEncoding (Server {..}) = pairs  ("oneOf" .= serverOneOf<>"type" .= serverType)


data SizeInMB = SizeInMB { 
    sizeInMBMaximum :: Int,
    sizeInMBMinimum :: Int,
    sizeInMBType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON SizeInMB where
  parseJSON (Object v) = SizeInMB <$> v .:  "maximum" <*> v .:  "minimum" <*> v .:  "type"
  parseJSON _          = mzero


instance ToJSON SizeInMB where
  toJSON     (SizeInMB {..}) = object ["maximum" .= sizeInMBMaximum, "minimum" .= sizeInMBMinimum, "type" .= sizeInMBType]
  toEncoding (SizeInMB {..}) = pairs  ("maximum" .= sizeInMBMaximum<>"minimum" .= sizeInMBMinimum<>"type" .= sizeInMBType)


data Readonly = Readonly { 
    readonlyType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Readonly where
  parseJSON (Object v) = Readonly <$> v .:  "type"
  parseJSON _          = mzero


instance ToJSON Readonly where
  toJSON     (Readonly {..}) = object ["type" .= readonlyType]
  toEncoding (Readonly {..}) = pairs  ("type" .= readonlyType)


data Options = Options { 
    optionsMinItems :: Int,
    optionsUniqueItems :: Bool,
    optionsItems :: Readonly,
    optionsType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Options where
  parseJSON (Object v) = Options <$> v .:  "minItems" <*> v .:  "uniqueItems" <*> v .:  "items" <*> v .:  "type"
  parseJSON _          = mzero


instance ToJSON Options where
  toJSON     (Options {..}) = object ["minItems" .= optionsMinItems, "uniqueItems" .= optionsUniqueItems, "items" .= optionsItems, "type" .= optionsType]
  toEncoding (Options {..}) = pairs  ("minItems" .= optionsMinItems<>"uniqueItems" .= optionsUniqueItems<>"items" .= optionsItems<>"type" .= optionsType)


data Properties = Properties { 
    propertiesFstype :: (Maybe (Type:|:[(Maybe Value)])),
    propertiesDevice :: (Maybe (Label:|:[(Maybe Value)])),
    propertiesRemotePath :: (Maybe (Label:|:[(Maybe Value)])),
    propertiesStorage :: (Maybe (Server:|:[(Maybe Value)])),
    propertiesServer :: (Maybe (Server:|:[(Maybe Value)])),
    propertiesSizeInMB :: (Maybe (SizeInMB:|:[(Maybe Value)])),
    propertiesOptions :: (Maybe (Options:|:[(Maybe Value)])),
    propertiesType :: (Maybe (Type:|:[(Maybe Value)])),
    propertiesReadonly :: (Maybe (Readonly:|:[(Maybe Value)])),
    propertiesLabel :: (Maybe (Label:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Properties where
  parseJSON (Object v) = Properties <$> v .:? "fstype" <*> v .:? "device" <*> v .:? "remotePath" <*> v .:? "storage" <*> v .:? "server" <*> v .:? "sizeInMB" <*> v .:? "options" <*> v .:? "type" <*> v .:? "readonly" <*> v .:? "label"
  parseJSON _          = mzero


instance ToJSON Properties where
  toJSON     (Properties {..}) = object ["fstype" .= propertiesFstype, "device" .= propertiesDevice, "remotePath" .= propertiesRemotePath, "storage" .= propertiesStorage, "server" .= propertiesServer, "sizeInMB" .= propertiesSizeInMB, "options" .= propertiesOptions, "type" .= propertiesType, "readonly" .= propertiesReadonly, "label" .= propertiesLabel]
  toEncoding (Properties {..}) = pairs  ("fstype" .= propertiesFstype<>"device" .= propertiesDevice<>"remotePath" .= propertiesRemotePath<>"storage" .= propertiesStorage<>"server" .= propertiesServer<>"sizeInMB" .= propertiesSizeInMB<>"options" .= propertiesOptions<>"type" .= propertiesType<>"readonly" .= propertiesReadonly<>"label" .= propertiesLabel)


data Tmpfs = Tmpfs { 
    tmpfsRequired :: [Text],
    tmpfsAdditionalProperties :: Bool,
    tmpfsProperties :: Properties:|:[(Maybe Value)]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Tmpfs where
  parseJSON (Object v) = Tmpfs <$> v .:  "required" <*> v .:  "additionalProperties" <*> v .:  "properties"
  parseJSON _          = mzero


instance ToJSON Tmpfs where
  toJSON     (Tmpfs {..}) = object ["required" .= tmpfsRequired, "additionalProperties" .= tmpfsAdditionalProperties, "properties" .= tmpfsProperties]
  toEncoding (Tmpfs {..}) = pairs  ("required" .= tmpfsRequired<>"additionalProperties" .= tmpfsAdditionalProperties<>"properties" .= tmpfsProperties)


data Definitions = Definitions { 
    definitionsDiskDevice :: Tmpfs,
    definitionsDiskUUID :: Tmpfs,
    definitionsNfs :: Tmpfs,
    definitionsTmpfs :: Tmpfs
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Definitions where
  parseJSON (Object v) = Definitions <$> v .:  "diskDevice" <*> v .:  "diskUUID" <*> v .:  "nfs" <*> v .:  "tmpfs"
  parseJSON _          = mzero


instance ToJSON Definitions where
  toJSON     (Definitions {..}) = object ["diskDevice" .= definitionsDiskDevice, "diskUUID" .= definitionsDiskUUID, "nfs" .= definitionsNfs, "tmpfs" .= definitionsTmpfs]
  toEncoding (Definitions {..}) = pairs  ("diskDevice" .= definitionsDiskDevice<>"diskUUID" .= definitionsDiskUUID<>"nfs" .= definitionsNfs<>"tmpfs" .= definitionsTmpfs)


data TopLevel = TopLevel { 
    topLevelRequired :: [Text],
    topLevelSchema :: Text,
    topLevelId :: Text,
    topLevelType :: Text,
    topLevelDescription :: Text,
    topLevelDefinitions :: Definitions,
    topLevelProperties :: Properties
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "required" <*> v .:  "$schema" <*> v .:  "id" <*> v .:  "type" <*> v .:  "description" <*> v .:  "definitions" <*> v .:  "properties"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["required" .= topLevelRequired, "$schema" .= topLevelSchema, "id" .= topLevelId, "type" .= topLevelType, "description" .= topLevelDescription, "definitions" .= topLevelDefinitions, "properties" .= topLevelProperties]
  toEncoding (TopLevel {..}) = pairs  ("required" .= topLevelRequired<>"$schema" .= topLevelSchema<>"id" .= topLevelId<>"type" .= topLevelType<>"description" .= topLevelDescription<>"definitions" .= topLevelDefinitions<>"properties" .= topLevelProperties)




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

