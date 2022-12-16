{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Arbitrary instances for the JSON @Value@.
module Data.Aeson.AutoType.Test (
    arbitraryTopValue
  ) where

import           Data.Aeson.AutoType.Pretty          () -- Generic instance for Value

import           Control.Applicative                 ((<$>), (<*>))
import           Data.Aeson
import           Data.Aeson.Key
import qualified Data.Aeson.KeyMap           as KV   (KeyMap(..), fromHashMapText, fromMapText, toMapText, fromHashMap, toHashMapText, fromList, toList)
import           Data.Function                       (on)
import           Data.Hashable                       (Hashable)
import           Data.List
import           Data.Scientific
import qualified Data.Text                   as Text
import           Data.Text                           (Text)
import qualified Data.Vector                 as V
import qualified Data.HashMap.Strict         as Map

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck
import           Test.SmallCheck.Series
import Data.Functor ((<&>))

instance Arbitrary Text where
  arbitrary = Text.pack  <$> sized (`vectorOf` alphabetic)
    where
      alphabetic = choose ('a', 'z')

instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance (Arbitrary v) => Arbitrary (Map.HashMap Text v) where
  arbitrary = makeMap <$> arbitrary

-- | Helper function for generating Arbitrary and Series instances
-- for @Data.HashMap.Strict.Map@ from lists of pairs.
makeMap :: (Ord a, Hashable a) =>[(a, b)] -> Map.HashMap a b
makeMap  = Map.fromList
         . nubBy  ((==)    `on` fst)
         . sortBy (compare `on` fst)

-- | Replace all keys because the Aeson Arbitrary instance generates characters that couldn't be used as haskell identifiers.
translateKeys :: Value -> Gen Value
translateKeys (Array a) = Array <$> mapM translateKeys a
translateKeys (Object o) = do
  hm <- KV.toList <$> mapM translateKeys o
  let x = fmap (\(k,v) -> (arbitrary,v)) hm
  y <- mapM (\(g,v) -> g >>= \k -> pure (fromText k,v)) x
  pure $ Object $ KV.fromList y
translateKeys x = pure x

-- | Generator for compound @Value@s
complexGens ::  Int -> [Gen Value]
complexGens i = [resize i arbitrary >>= translateKeys . Object,
                 resize i arbitrary >>= translateKeys . Array]

-- | Arbitrary JSON (must start with Object or Array.)
arbitraryTopValue :: Gen Value
arbitraryTopValue  = sized $ oneof . complexGens

-- * SmallCheck Serial instances
instance Monad m => Serial m Text where
  series = newtypeCons Text.pack

instance Monad m => Serial m Scientific where
  series = cons2 scientific

instance Serial m a => Serial m (V.Vector a) where
  series = newtypeCons V.fromList

instance Serial m v => Serial m (KV.KeyMap v) where
  series = newtypeCons (KV.fromHashMapText . makeMap)

-- This one is generated with Generics and instances above
instance Monad m => Serial m Value
