module Data.Aeson.AutoType.Util where

-- Missing instances
instance Hashable a => Hashable (Set a) where
  hashWithSalt = Set.foldr (flip hashWithSalt)

 
 
