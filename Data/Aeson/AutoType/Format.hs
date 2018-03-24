module Data.Aeson.AutoType.Format(capitalize, uncapitalize) where

import Data.Text(Text)
import qualified Data.Text as Text

capitalize :: Text -> Text
capitalize word = Text.toUpper first `Text.append` rest
  where
    (first, rest) = Text.splitAt 1 word

uncapitalize :: Text -> Text
uncapitalize word = Text.toLower first `Text.append` rest
  where
    (first, rest) = Text.splitAt 1 word
