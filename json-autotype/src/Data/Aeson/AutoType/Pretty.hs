{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Instances of @Text.PrettyPrint.Out@ class to visualize
-- Aeson @Value@ data structure.
module Data.Aeson.AutoType.Pretty() where

import qualified Data.HashMap.Strict as Hash
import           Data.HashMap.Strict(HashMap)
import           Data.Aeson
import           Data.Aeson.KeyMap(KeyMap, toHashMapText)
import           Data.Aeson.Key
import qualified Data.Text                  as Text
import           Data.Text                 (Text)
import           Data.Set                   as Set(Set, toList)
import           Data.Scientific
import           Data.Vector                as V(Vector, toList)
import           Text.PrettyPrint.GenericPretty
import           Text.PrettyPrint

formatPair :: (Out a, Out b) => (a, b) -> Doc
formatPair (a, b) = nest 1 (doc a <+> ": " <+> doc b <+> ",")

-- * This is to make prettyprinting possible for Aeson @Value@ type.
instance Out Scientific where
  doc = doc . show
  docPrec _ = doc

instance (Out a) => Out (Vector a) where
  doc (V.toList -> v) = "<" <+> doc v <+> ">"
  docPrec _ = doc

instance Out Value

instance (Out a) => Out (KeyMap a) where
  doc     (toHashMapText -> s) = doc s
  docPrec _                    = doc

instance (Out a) => Out (Set a) where
  doc     (Set.toList -> s) = "{" <+> doc s <+> "}"
  docPrec _                 = doc

instance (Out a, Out b) => Out (HashMap a b) where
  doc (Hash.toList -> dict) = foldl ($$) "{" (map formatPair dict) $$ nest 1 "}"
  docPrec _ = doc

instance Out Text where
  doc       = text . Text.unpack -- TODO: check if there may be direct way?
  docPrec _ = doc
