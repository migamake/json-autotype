{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, OverloadedStrings #-}
module Main where

import           Control.Lens.TH
import           Control.Lens
import           Control.Monad    (forM, forM_)
import           Control.Exception(assert)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as Map
import qualified Data.Set            as Set
import qualified Data.Vector         as V
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text           as Text
import qualified Data.Text.IO        as Text
import           Data.Text          (Text)
import           Data.Set           (Set )
import           Data.List          (sort, foldl1')
import           Data.Ord           (Ord(..), comparing)
import           Data.Char          (isAlpha)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict(State, runState)

import           Data.Aeson.AutoType.Extract

data DeclState = DeclState { _decls   :: [Text]
                           , _counter :: Int
                           }
  deriving (Eq, Show, Ord)

makeLenses ''DeclState

type DeclM = State DeclState

type Map k v = Map.HashMap k v 

stepM :: DeclM Int
stepM = counter %%= (\i -> (i, i+1))

tShow :: (Show a) => a -> Text
tShow = Text.pack . show 

wrapDecl identifier contents = Text.unlines [header, contents, "  }"]
  where
    header = Text.concat ["data ", identifier, " = ", identifier, " { "]

genericIdentifier = do
  i <- stepM
  return $! ("Obj" `Text.append` tShow i)

-- * Naive type printing.
newDecl :: Text -> [(Text, Type)] -> DeclM Text
newDecl identifier kvs = do attrs <- forM kvs $ \(k, v) -> do
                              formatted <- formatType' v
                              return (k, formatted)
                            let decl = wrapDecl identifier $ fieldDecls attrs
                            decls %%= (\ds -> ((), decl:ds))
                            return identifier
  where
    fieldDecls attrList = Text.unlines $ map fieldDecl attrList
    fieldDecl  :: (Text, Text) -> Text
    fieldDecl (name, fType) = Text.concat ["    ", name, " :: ", fType]

emptySetLikes = Set.fromList [TNull, TArray $ TUnion $ Set.fromList []]

formatType' :: Type -> DeclM Text
formatType'  TString                          = return "Text"
formatType'  TNum                             = return "Int"
formatType'  TBool                            = return "Bool"
formatType' (TLabel l)                        = return $ normalizeTypeName l
formatType' (TUnion u) | uu <- u `Set.difference` emptySetLikes,
                         Set.size uu == 1     = do fmt <- formatType' $ head $ Set.toList u
                                                   return $ "Maybe " `Text.append` fmt
formatType' (TUnion u)                        = do tys <- forM (Set.toList u) formatType'
                                                   return $ Text.concat ["(",
                                                                         "\n|" `Text.intercalate` tys,
                                                                         ")"]
formatType' (TArray a)                        = do inner <- formatType' a
                                                   return $ Text.concat ["[", inner, "]"]
formatType' (TObj   o)                        = do ident <- genericIdentifier
                                                   newDecl ident d
  where
    d = Map.toList $ unDict o 
formatType'  TNull                            = return "Maybe Text"
formatType'  e | e `Set.member` emptySetLikes = return "Maybe Text"
formatType'  t                                = return $ "ERROR: Don't know how to handle: " `Text.append` tShow t


formatType = runDecl . formatType'

runDecl decl = Text.unlines $ finalState ^. decls
  where
    initialState    = DeclState [] 1
    (_, finalState) = runState decl initialState

-- * Splitting object types by label for unification.
type TypeTree    = Map Text [Type]

type TypeTreeM a = State TypeTree a

addType :: Text -> Type -> TypeTreeM ()
addType label typ = modify (Map.insertWith (++) label [typ])

splitTypeByLabel' :: Text -> Type -> TypeTreeM Type
splitTypeByLabel' l  TString   = return TString
splitTypeByLabel' l  TNum      = return TNum
splitTypeByLabel' l  TBool     = return TBool
splitTypeByLabel' l (TLabel r) = assert False $ return $ TLabel r -- unnecessary?
splitTypeByLabel' l (TUnion u) = do m <- mapM (splitTypeByLabel' l) $ Set.toList u
                                    return $! TUnion $ Set.fromList m
splitTypeByLabel' l (TArray a) = do m <- splitTypeByLabel' (l `Text.append` "Elt") a
                                    return $! TArray m
splitTypeByLabel' l (TObj   o) = do kvs <- forM d $ \(k, v) -> do
                                       component <- splitTypeByLabel' k v
                                       return (k, component)
                                    addType l (TObj $ Dict $ Map.fromList kvs)
                                    return $! TLabel l
  where
    d = Map.toList $ unDict o 
splitTypeByLabel' l TNull      = return TNull
splitTypeByLabel' l t          = error $ "ERROR: Don't know how to handle: " ++ show t

splitTypeByLabel :: Text -> Type -> Map Text Type
splitTypeByLabel topLabel t = Map.map (foldl1' unifyTypes) finalState
  where
    job = do r <- splitTypeByLabel' topLabel t
             addType topLabel r
    initialState    = Map.empty
    (_, finalState) = runState job initialState

formatObjectType identifier (TObj o) = newDecl identifier d
  where
    d = Map.toList $ unDict o
formatObjectType identifier other    = formatType' other

displaySplitTypes dict = runDecl decls
  where
    decls =
      forM (Map.toList dict) $ \(name, typ) -> do
        let name' = normalizeTypeName name
        content <- formatObjectType name' typ
        return $! Text.concat ["-- ", name', ":\n",
                               wrapDecl name' content]

normalizeTypeName :: Text -> Text
normalizeTypeName = Text.concat              .
                    map capitalize           .
                    filter (not . Text.null) .
                    Text.split (not . isAlpha)

capitalize :: Text -> Text
capitalize word = Text.toUpper first `Text.append` rest
  where
    (first, rest) = Text.splitAt 1 word

main = do bs <- BSL.readFile "test/test.json"
          let Just v = decode bs
          -- print (v :: Value)
          let t = extractType v
          --print t
          --putStrLn "Bungwa!!!"
          Text.putStrLn $ formatType t
          print $ valueSize v
          print $ valueTypeSize v
          print $ typeSize t
          let splitted = splitTypeByLabel "TopLevel" t
          --print splitted
          let result = displaySplitTypes splitted
          Text.putStrLn result
          {-
          forM_ (Map.toList splitted) $ \(label, types) -> do
            Text.putStr label
            putStr " : "
            putStrLn $ show types
            putStr " -> -> -> -> -> "
            --print $ foldl1' unifyTypes types
            forM_ (zip types $ tail types) $ \(t1, t2) -> do
              putStrLn $ show t1 ++ " <> " ++ show t2
              print $ t1 `unifyTypes` t2-}

          


