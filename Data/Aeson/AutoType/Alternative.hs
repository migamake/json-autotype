{-# LANGUAGE ViewPatterns #-}
module Data.Aeson.AutoType.Alternative(
    Alt(..)
  , toEither, fromEither
  ) where

import Data.Aeson
import Control.Applicative

data Alt a b = FirstAlt  a
             | SecondAlt b
  deriving(Show,Eq,Ord)

toEither :: Alt a b -> Either a b
toEither (FirstAlt  a) = Left  a
toEither (SecondAlt b) = Right b

fromEither :: Either a b -> Alt a b
fromEither (Left  a) = FirstAlt  a
fromEither (Right b) = SecondAlt b

instance (ToJSON a, ToJSON b) => ToJSON (Alt a b) where
    toJSON (FirstAlt  a) = toJSON a
    toJSON (SecondAlt b) = toJSON b
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (Alt a b) where
    parseJSON input = (FirstAlt  <$> parseJSON input) <|>
                      (SecondAlt <$> parseJSON input) <|>
                      fail ("Neither alternative was found for: " ++ show input)
    {-# INLINE parseJSON #-}
