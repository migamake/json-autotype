module Data.Aeson.Plugin.Interface.SubType (
    SubTypePlugin (..)
  ) where

import Data.Aeson.AutoType.Type
import Data.Aeson
import Data.Dynamic

-- | Hmm... this should be existential type?
type TypeDesc = String

data SubTypePlugin = SubTypePlugin {
    detect :: [Value]  -> Maybe SubTypeDesc -- | Check whether a set of values belongs to this type family
  , unify  :: SubTypeDesc -> SubTypeDesc -> Either SubTypeDesc Type
  }

-- | Description of a subtype
data SubTypeDesc = SubTypeDesc {
    subtypeName  :: String           -- | Code that is different for different type families
  , subtypeClass :: Type
  , reference    :: String -> String -- | Show type reference with a given name prefix
  , declare      :: String           -- | Show type declaration
  , typeInfo     :: Dynamic
  }
