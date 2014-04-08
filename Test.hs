{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Lens.TH
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as Hash
import qualified Data.Set            as Set
import qualified Data.Vector         as V
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text          (Text)
import           Data.Set           (Set )
import           Data.HashMap.Strict(HashMap)
import           Data.List          (sort, foldl1')
import           Data.Ord           (Ord(..), comparing)

import           Data.Aeson.AutoType.Extract

{-
data Value
  = Object !Object
  | Array !Array
  | String !Data.Text.Internal.Text
  | Number !Data.Scientific.Scientific
  | Bool !Bool
  | Null
 -}

data DeclState = DeclState_ { decls_   :: [Text]
                            , counter_ :: Int
                            }
  deriving (Eq, Show, Ord)

makeLenses ''DeclState

{-
formatType' :: Type -> CounterM [Text]
formatType' (i, TNull   )                    = ["Maybe Text"]
formatType' (i, TString )                    = ["Text"]
formatType' (i, TNum    )                    = ["Int"]
formatType' (i, TUnion u) | uu <- u `Set.difference` Set.singleton TNull,
                      Set.size uu == 1 =
  ["Maybe " `Text.append` formatType u]
formatType (i, TArray a)                    = [Text.concat ["[", formatType (i, a), "]"]]
formatType (i, TObj   o)                    = Text.concat $ ["{"] ++ ++ ["}"]
-}
main = do bs <- BSL.readFile "test/test.json"
          let Just v = decode bs
          -- print (v :: Value)
          let t = extractType v
          print t
          putStrLn "Bungwa!!!"
          print $ valueSize v
          print $ valueTypeSize v
          print $ typeSize t

