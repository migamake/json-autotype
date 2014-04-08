{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import           Control.Lens.TH
import           Control.Lens
import           Control.Monad(forM)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as Hash
import qualified Data.Set            as Set
import qualified Data.Vector         as V
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           Data.Text          (Text)
import           Data.Set           (Set )
import           Data.HashMap.Strict(HashMap)
import           Data.List          (sort, foldl1')
import           Data.Ord           (Ord(..), comparing)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict(State, runState)

import           Data.Aeson.AutoType.Extract

data DeclState = DeclState { _decls   :: [Text]
                           , _counter :: Int
                           }
  deriving (Eq, Show, Ord)

makeLenses ''DeclState

type DeclM = State DeclState

stepM :: DeclM Int
stepM = counter %%= (\i -> (i, i+1))

tShow :: (Show a) => a -> Text
tShow = Text.pack . show 

newDecl :: [(Text, Text)] -> DeclM Text
newDecl kvList = do i          :: Int <- stepM
                    let identifier :: Text = "Obj" `Text.append` tShow i
                        header = Text.concat ["data ", identifier, " = ", identifier, " { "]
                        decl   = Text.unlines $ [header] ++ fieldDecls ++ ["  }"]
                    decls %%= (\ds -> ((), decl:ds))
                    return identifier
  where
    fieldDecls :: [Text]
    fieldDecls = map fieldDecl kvList
    fieldDecl  :: (Text, Text) -> Text
    fieldDecl (name, fType) = Text.concat ["    ", name, " : ", fType]

formatType' :: Type -> DeclM Text
formatType'  TString                      = return "Text"
formatType'  TNum                         = return "Int"
formatType'  TBool                        = return "Bool"
formatType' (TUnion u) | uu <- u `Set.difference` Set.singleton TNull,
                         Set.size uu == 1 = do fmt <- formatType' $ head $ Set.toList u
                                               return $ "Maybe " `Text.append` fmt
formatType' (TArray a)                    = do inner <- formatType' a
                                               return $ Text.concat ["[", inner, "]"]
formatType' (TObj   o)                    = do kvs <- forM d $ \(k, v) -> do
                                                        formatted <- formatType' v
                                                        return (k, formatted)
                                               newDecl kvs
  where
    d = Hash.toList $ unDict o 
formatType'  TNull                        = return "Maybe Text"
formatType'  e         | e == emptyType   = return "Maybe Text"
formatType' t                             = return $ "ERROR: Don't know how to handle: " `Text.append` tShow t

formatType t = Text.unlines $ finalState ^. decls
  where
    initialState    = DeclState [] 1
    (_, finalState) = runState (formatType' t) initialState

main = do bs <- BSL.readFile "test/test.json"
          let Just v = decode bs
          -- print (v :: Value)
          let t = extractType v
          print t
          putStrLn "Bungwa!!!"
          Text.putStrLn $ formatType t
          print $ valueSize v
          print $ valueTypeSize v
          print $ typeSize t

