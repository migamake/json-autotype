{-# LANGUAGE TypeOperators #-}
-- | This module defines data type (a :|: b) that behaves all like @Either@,
-- except that has no tag in JSON representation as used by @FromJSON@ and @ToJSON@.
module Data.Aeson.AutoType.Alternative(
    (:|:)(..)
  , toEither, fromEither
  , alt
  ) where

import Data.Aeson
import Control.Applicative

-- | Data type (a :|: b) that behaves all like @Either@,
-- except that has no tag in JSON representation as used by @FromJSON@ and @ToJSON@.
data a :|: b = AltLeft  a
             | AltRight b
  deriving(Show,Eq,Ord)
infixr 5 :|:

-- | Convert to @Either@ datatype.
toEither :: a :|: b -> Either a b
toEither (AltLeft  a) = Left  a
toEither (AltRight b) = Right b
{-# INLINE toEither #-}

-- | Convert from @Either@ datatype.
fromEither :: Either a b -> a :|: b
fromEither (Left  a) = AltLeft  a
fromEither (Right b) = AltRight b
{-# INLINE fromEither #-}

-- | Deconstruct the type with two functions corresponding to constructors.
-- This is like @either@.
alt :: (a -> c) -> (b -> c) -> a :|: b -> c
alt f _ (AltLeft  a) = f a
alt _ g (AltRight b) = g b
infixr 5 `alt`

instance (ToJSON a, ToJSON b) => ToJSON (a :|: b) where
    toJSON (AltLeft  a) = toJSON a
    toJSON (AltRight b) = toJSON b
    {-# INLINE toJSON #-}

instance (FromJSON a, FromJSON b) => FromJSON (a :|: b) where
    parseJSON input = (AltLeft  <$> parseJSON input) <|>
                      (AltRight <$> parseJSON input) <|>
                      fail ("Neither alternative was found for: " ++ show input)
    {-# INLINE parseJSON #-}
