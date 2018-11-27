
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DeriveGeneric       #-}

module ExamplesJsonSchemaProductSchema where

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

data Name = Name { 
    nameType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Name where
  parseJSON (Object v) = Name <$> v .:  "type"
  parseJSON _          = mzero


instance ToJSON Name where
  toJSON     (Name {..}) = object ["type" .= nameType]
  toEncoding (Name {..}) = pairs  ("type" .= nameType)


data Id = Id { 
    idType :: Text,
    idDescription :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Id where
  parseJSON (Object v) = Id <$> v .:  "type" <*> v .:  "description"
  parseJSON _          = mzero


instance ToJSON Id where
  toJSON     (Id {..}) = object ["type" .= idType, "description" .= idDescription]
  toEncoding (Id {..}) = pairs  ("type" .= idType<>"description" .= idDescription)


data Price = Price { 
    priceMinimum :: Int,
    priceExclusiveMinimum :: Bool,
    priceType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Price where
  parseJSON (Object v) = Price <$> v .:  "minimum" <*> v .:  "exclusiveMinimum" <*> v .:  "type"
  parseJSON _          = mzero


instance ToJSON Price where
  toJSON     (Price {..}) = object ["minimum" .= priceMinimum, "exclusiveMinimum" .= priceExclusiveMinimum, "type" .= priceType]
  toEncoding (Price {..}) = pairs  ("minimum" .= priceMinimum<>"exclusiveMinimum" .= priceExclusiveMinimum<>"type" .= priceType)


data WarehouseLocation = WarehouseLocation { 
    warehouseLocationRef :: Text,
    warehouseLocationDescription :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON WarehouseLocation where
  parseJSON (Object v) = WarehouseLocation <$> v .:  "$ref" <*> v .:  "description"
  parseJSON _          = mzero


instance ToJSON WarehouseLocation where
  toJSON     (WarehouseLocation {..}) = object ["$ref" .= warehouseLocationRef, "description" .= warehouseLocationDescription]
  toEncoding (WarehouseLocation {..}) = pairs  ("$ref" .= warehouseLocationRef<>"description" .= warehouseLocationDescription)


data Dimensions = Dimensions { 
    dimensionsRequired :: [Text],
    dimensionsType :: Text,
    dimensionsProperties :: Properties
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Dimensions where
  parseJSON (Object v) = Dimensions <$> v .:  "required" <*> v .:  "type" <*> v .:  "properties"
  parseJSON _          = mzero


instance ToJSON Dimensions where
  toJSON     (Dimensions {..}) = object ["required" .= dimensionsRequired, "type" .= dimensionsType, "properties" .= dimensionsProperties]
  toEncoding (Dimensions {..}) = pairs  ("required" .= dimensionsRequired<>"type" .= dimensionsType<>"properties" .= dimensionsProperties)


data Tags = Tags { 
    tagsMinItems :: Int,
    tagsUniqueItems :: Bool,
    tagsItems :: Items,
    tagsType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Tags where
  parseJSON (Object v) = Tags <$> v .:  "minItems" <*> v .:  "uniqueItems" <*> v .:  "items" <*> v .:  "type"
  parseJSON _          = mzero


instance ToJSON Tags where
  toJSON     (Tags {..}) = object ["minItems" .= tagsMinItems, "uniqueItems" .= tagsUniqueItems, "items" .= tagsItems, "type" .= tagsType]
  toEncoding (Tags {..}) = pairs  ("minItems" .= tagsMinItems<>"uniqueItems" .= tagsUniqueItems<>"items" .= tagsItems<>"type" .= tagsType)


data Properties = Properties { 
    propertiesLength :: (Maybe (Name:|:[(Maybe Value)])),
    propertiesHeight :: (Maybe (Name:|:[(Maybe Value)])),
    propertiesWidth :: (Maybe (Name:|:[(Maybe Value)])),
    propertiesName :: (Maybe (Name:|:[(Maybe Value)])),
    propertiesId :: (Maybe (Id:|:[(Maybe Value)])),
    propertiesPrice :: (Maybe (Price:|:[(Maybe Value)])),
    propertiesWarehouseLocation :: (Maybe (WarehouseLocation:|:[(Maybe Value)])),
    propertiesDimensions :: (Maybe (Dimensions:|:[(Maybe Value)])),
    propertiesTags :: (Maybe (Tags:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Properties where
  parseJSON (Object v) = Properties <$> v .:? "length" <*> v .:? "height" <*> v .:? "width" <*> v .:? "name" <*> v .:? "id" <*> v .:? "price" <*> v .:? "warehouseLocation" <*> v .:? "dimensions" <*> v .:? "tags"
  parseJSON _          = mzero


instance ToJSON Properties where
  toJSON     (Properties {..}) = object ["length" .= propertiesLength, "height" .= propertiesHeight, "width" .= propertiesWidth, "name" .= propertiesName, "id" .= propertiesId, "price" .= propertiesPrice, "warehouseLocation" .= propertiesWarehouseLocation, "dimensions" .= propertiesDimensions, "tags" .= propertiesTags]
  toEncoding (Properties {..}) = pairs  ("length" .= propertiesLength<>"height" .= propertiesHeight<>"width" .= propertiesWidth<>"name" .= propertiesName<>"id" .= propertiesId<>"price" .= propertiesPrice<>"warehouseLocation" .= propertiesWarehouseLocation<>"dimensions" .= propertiesDimensions<>"tags" .= propertiesTags)


data Items = Items { 
    itemsRequired :: (Maybe ([Text])),
    itemsTitle :: (Maybe (Text:|:[(Maybe Value)])),
    itemsType :: Text,
    itemsProperties :: (Maybe (Properties:|:[(Maybe Value)]))
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Items where
  parseJSON (Object v) = Items <$> v .:? "required" <*> v .:? "title" <*> v .:  "type" <*> v .:? "properties"
  parseJSON _          = mzero


instance ToJSON Items where
  toJSON     (Items {..}) = object ["required" .= itemsRequired, "title" .= itemsTitle, "type" .= itemsType, "properties" .= itemsProperties]
  toEncoding (Items {..}) = pairs  ("required" .= itemsRequired<>"title" .= itemsTitle<>"type" .= itemsType<>"properties" .= itemsProperties)


data TopLevel = TopLevel { 
    topLevelItems :: Items,
    topLevelSchema :: Text,
    topLevelTitle :: Text,
    topLevelType :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON TopLevel where
  parseJSON (Object v) = TopLevel <$> v .:  "items" <*> v .:  "$schema" <*> v .:  "title" <*> v .:  "type"
  parseJSON _          = mzero


instance ToJSON TopLevel where
  toJSON     (TopLevel {..}) = object ["items" .= topLevelItems, "$schema" .= topLevelSchema, "title" .= topLevelTitle, "type" .= topLevelType]
  toEncoding (TopLevel {..}) = pairs  ("items" .= topLevelItems<>"$schema" .= topLevelSchema<>"title" .= topLevelTitle<>"type" .= topLevelType)




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

