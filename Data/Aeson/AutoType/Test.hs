{-# LANGUAGE FlexibleInstances #-}
-- | Arbitrary instances for the JSON @Value@.
module Data.Aeson.AutoType.Test (
    arbitraryTopValue
  ) where

import           Control.Applicative                 ((<$>))
import           Data.Aeson
import           Data.Function                       (on)
import           Data.Generics.Uniplate.Data
import           Data.List
import           Data.Scientific
import qualified Data.Text                   as Text
import           Data.Text                           (Text)
import qualified Data.Vector                 as V
import qualified Data.HashMap.Strict         as Map

import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck

instance Arbitrary Text where
  arbitrary = Text.pack  <$> sized (`vectorOf` alphabetic)
    where
      alphabetic = choose ('a', 'z')

instance (Arbitrary a) => Arbitrary (V.Vector a) where
  arbitrary = V.fromList <$> arbitrary

instance (Arbitrary v) => Arbitrary (Map.HashMap Text v) where
  arbitrary = makeMap <$> arbitrary
    where
      makeMap  = Map.fromList
               . nubBy  ((==)    `on` fst)
               . sortBy (compare `on` fst)

instance Arbitrary Scientific where
  arbitrary = scientific <$> arbitrary <*> arbitrary

-- TODO: top value has to be complex: Object or Array
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

simpleShrink :: Value -> [Value]
simpleShrink (Array  a) = map (Array  .   V.fromList) $ shrink $ V.toList   a
simpleShrink (Object o) = map (Object . Map.fromList) $ shrink $ Map.toList o
simpleShrink _          = [] -- Nothing for simple objects

-- | Generators of compound structures: object and array.
complexGens i = [Object . Map.fromList <$> resize i arbitrary,
                 Array                 <$> resize i arbitrary]

-- | Arbitrary JSON (must start with Object or Array.)
arbitraryTopValue :: Gen Value
arbitraryTopValue = sized $ oneof . complexGens

