{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | Arbitrary instances for the JSON @Value@.
module Data.Aeson.AutoType.Test (
    arbitraryTopValue
  ) where

import           Data.Aeson.AutoType.Pretty          () -- Generic instance for Value

import           Control.Applicative                 ((<$>), (<*>))
import           Data.Aeson
import           Data.Function                       (on)
import           Data.Hashable                       (Hashable)
import           Data.Generics.Uniplate.Data
import           Data.List
import           Data.Scientific
import qualified Data.Text                   as Text
import           Data.Text                           (Text)
import qualified Data.Vector                 as V
import qualified Data.HashMap.Strict         as Map
import           GHC.Generics

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck
import           Test.SmallCheck.Series

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

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary

-- TODO: top value has to be complex: Object or Array
-- TODO: how to accumulate cost when generating the series?
instance Arbitrary Value where
  arbitrary = sized arb
    where
      arb n | n < 0 = error "Negative size!"
      arb 0         = return Null
      arb 1         = oneof                          simpleGens
      arb i         = oneof $ complexGens (i - 1) ++ simpleGens
      simpleGens    = [Number <$> arbitrary
                      ,Bool   <$> arbitrary
                      ,String <$> arbitrary]
  shrink = concatMap simpleShrink
         . universe

-- | Transformation to shrink top level of @Value@, doesn't consider nested sub-@Value@s.
simpleShrink           :: Value -> [Value]
simpleShrink (Array  a) = map (Array  .   V.fromList) $ shrink $ V.toList   a
simpleShrink (Object o) = map (Object . Map.fromList) $ shrink $ Map.toList o
simpleShrink _          = [] -- Nothing for simple objects

-- | Generator for compound @Value@s
complexGens ::  Int -> [Gen Value]
complexGens i = [Object . Map.fromList <$> resize i arbitrary,
                 Array                 <$> resize i arbitrary]

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

instance Serial m v => Serial m (Map.HashMap Text v) where
  series = newtypeCons makeMap

-- This one is generated with Generics and instances above
instance Monad m => Serial m Value
