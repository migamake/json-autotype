module Data.Aeson.AutoType.Util where

import Data.Hashable
import qualified Data.Set as Set

-- Missing instances
instance Hashable a => Hashable (Set.Set a) where
  hashWithSalt = Set.foldr (flip hashWithSalt)

 
 
