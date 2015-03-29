-- | API to which @SubtypePlugin@s should conform.
module Data.Aeson.AutoType.Plugin.Subtype (
    SubtypePlugin (..)
  , SubtypeDesc   (..)
  ) where

import Data.Aeson.AutoType.Type
import Data.Aeson
import Data.Dynamic

-- | Hmm... this should be existential type?
type TypeDesc = String

-- | Operations that @SubtypPlugin@ must implement.
data SubtypePlugin = SubtypePlugin {
    detect :: [Value]     -> Maybe SubtypeDesc -- | Check whether a set of values belongs to this type family
  , unify  :: SubtypeDesc -> SubtypeDesc -> Either SubtypeDesc Type
  }

-- | Description of a subtype
data SubtypeDesc = SubtypeDesc {
    subtypeName  :: String           -- | Code that is different for different type families
  , subtypeClass :: Type
  , reference    :: String -> String -- | Show type reference with a given name prefix
  , declare      :: String           -- | Show type declaration
  , typeInfo     :: Dynamic
  }
