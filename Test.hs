{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import           Control.Arrow             ((&&&))
import           Control.Lens.TH
import           Control.Lens
import           Control.Monad             (forM, forM_)
import           Control.Exception(assert)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict        as Map
import qualified Data.Set                   as Set
import qualified Data.Vector                as V
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Data.Text                 (Text)
import           Data.Set                  (Set )
import           Data.List                 (sort, foldl1')
import           Data.Ord                  (Ord(..), comparing)
import           Data.Char                 (isAlpha)
import           Data.Tuple.Utils          (fst3)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict(State, runState)
import           Data.Hashable             (Hashable(..))
import qualified Data.Graph          as Graph

import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Util
import           Data.Aeson.AutoType.Format

assertM v = assert v $ return ()

header = Text.unlines ["{-# LANGUAGE TemplateHaskell #-}"
                      ,"module JSONTypes where"
                      ,""
                      ,"import           Data.Text (Text)"
                      ,"import           Data.Aeson(decode, Value(..), FromJSON(..),"
                      ,"                            (.:), (.:?), (.!=))"
                      ,"import           Data.Aeson.TH"
                      ,""]


main = do bs <- BSL.readFile "test/test.json"
          let Just v = decode bs
          let t = extractType v
          let splitted = splitTypeByLabel "TopLevel" t
          Text.putStrLn header
          let result = displaySplitTypes splitted
          --Text.putStrLn result
          assertM $ not $ any hasNonTopTObj $ Map.elems splitted
          putStr "--"
          --print $ map fst $ toposort splitted
          let uCands = unificationCandidates splitted
          forM_ uCands $ \cs -> do
            putStr "-- "
            Text.putStrLn $ "=" `Text.intercalate` cs
          let unified = unifyCandidates uCands splitted
          Text.putStrLn $ displaySplitTypes unified

