{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Formatting type declarations and class instances for inferred types.

module Data.Aeson.AutoType.CodeGen.PureScriptFormat(
  displaySplitTypes,
  normalizeTypeName) where

import           Control.Applicative         ((<$>), (<*>))
import           Control.Arrow               ((&&&))
import           Control.Exception           (assert)
import           Control.Lens
import           Control.Lens.TH
import           Control.Monad               (forM)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict  (State, runState)
import           Data.Char                   (isAlpha, isDigit)
import qualified Data.HashMap.Strict         as Map
import           Data.List                   (foldl1')
import           Data.Monoid
import           Data.Set                    (Set, toList)
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import           GHC.Generics                (Generic)

import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Format
import           Data.Aeson.AutoType.Split
import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Util    ()

--------------------------------------------------------------------------------
