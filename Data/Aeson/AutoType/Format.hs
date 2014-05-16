{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, OverloadedStrings #-}
module Data.Aeson.AutoType.Format(
  displaySplitTypes, splitTypeByLabel, unificationCandidates,
  unifyCandidates
) where

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
--import           Data.Tuple.Utils          (fst3)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict(State, runState)
import           Data.Hashable             (Hashable(..))
import qualified Data.Graph          as Graph

import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Util

fst3 (a, _b, _c) = a

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

wrapDecl identifier contents = Text.unlines [header, contents, "  } deriving (Show,Eq)"
                                            ,"\nderiveJSON defaultOptions ''" `Text.append` identifier]
  where
    header = Text.concat ["data ", identifier, " = ", identifier, " { "]

-- | Makes a generic identifier name.
genericIdentifier = do
  i <- stepM
  return $! "Obj" `Text.append` tShow i

-- * Naive type printing.
newDecl :: Text -> [(Text, Type)] -> DeclM Text
newDecl identifier kvs = do attrs <- forM kvs $ \(k, v) -> do
                              formatted <- formatType' v
                              return (k, formatted)
                            let decl = wrapDecl identifier $ fieldDecls attrs
                            decls %%= (\ds -> ((), decl:ds))
                            return identifier
  where
    fieldDecls attrList = Text.intercalate ",\n" $ map fieldDecl attrList
    fieldDecl  :: (Text, Text) -> Text
    fieldDecl (name, fType) = Text.concat ["    ", normalizeFieldName identifier name, " :: ", fType]

normalizeFieldName identifier = escapeKeywords             .
                                uncapitalize               .
                                (normalizeTypeName identifier `Text.append`) .
                                normalizeTypeName

keywords = Set.fromList ["type", "data", "module"]

escapeKeywords k | k `Set.member` keywords = k `Text.append` "_"
escapeKeywords k                           = k

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
splitTypeByLabel' l  TNull     = return TNull
splitTypeByLabel' l (TLabel r) = assert False $ return $ TLabel r -- unnecessary?
splitTypeByLabel' l (TUnion u) = do m <- mapM (splitTypeByLabel' l) $ Set.toList u
                                    return $! TUnion $! Set.fromList m
splitTypeByLabel' l (TArray a) = do m <- splitTypeByLabel' (l `Text.append` "Elt") a
                                    return $! TArray m
splitTypeByLabel' l (TObj   o) = do kvs <- forM d $ \(k, v) -> do
                                       component <- splitTypeByLabel' k v
                                       return (k, component)
                                    addType l (TObj $ Dict $ Map.fromList kvs)
                                    return $! TLabel l
  where
    d = Map.toList $ unDict o 
--splitTypeByLabel' l  t         = error $ "ERROR: Don't know how to handle: " ++ show t

splitTypeByLabel :: Text -> Type -> Map Text Type
splitTypeByLabel topLabel t = Map.map (foldl1' unifyTypes) finalState
  where
    job = splitTypeByLabel' topLabel t
          --   addType topLabel r
    initialState    = Map.empty
    (_, finalState) = runState job initialState

formatObjectType identifier (TObj o) = newDecl identifier d
  where
    d = Map.toList $ unDict o
formatObjectType identifier other    = formatType' other

displaySplitTypes dict = runDecl decls
  where
    decls =
      forM (toposort dict) $ \(name, typ) -> do
        let name' = normalizeTypeName name
        formatObjectType name' typ

normalizeTypeName :: Text -> Text
normalizeTypeName = escapeKeywords           .
                    Text.concat              .
                    map capitalize           .
                    filter (not . Text.null) .
                    Text.split (not . isAlpha)

capitalize :: Text -> Text
capitalize word = Text.toUpper first `Text.append` rest
  where
    (first, rest) = Text.splitAt 1 word

uncapitalize :: Text -> Text
uncapitalize word = Text.toLower first `Text.append` rest
  where
    (first, rest) = Text.splitAt 1 word

-- | Topological sorting of splitted types so that it is accepted declaration order.
toposort :: Map Text Type -> [(Text, Type)]  
toposort splitted = map ((id &&& (splitted Map.!)) . fst3 . graphKey) $ Graph.topSort graph
  where
    (graph, graphKey) = Graph.graphFromEdges' $ map makeEntry $ Map.toList splitted
    makeEntry (k, v) = (k, k, allLabels v)

-- | Computes all type labels referenced by a given type.
allLabels :: Type -> [Text]
allLabels = flip go []
  where
    go (TLabel l) ls = l:ls
    go (TArray t) ls = go t ls
    go (TUnion u) ls = Set.foldr go ls          u
    go (TObj   o) ls = Map.foldr go ls $ unDict o
    go other      ls = ls

-- * Finding candidates for extra unifications
-- | For a given splitted types, it returns candidates for extra
-- unifications.
unificationCandidates = Map.elems             .
                        Map.filter candidates .
                        Map.fromListWith (++) .
                        map entry             .
                        Map.toList
  where
    candidates [ ] = False
    candidates [a] = False
    candidates _   = True
    entry (k, TObj o) = (Set.fromList $ Map.keys $ unDict o, [k])
    entry (_, other ) = error $ "Unexpected type: " ++ show other

-- | Unifies candidates on a give input list.
unifyCandidates :: [[Text]] -> Map Text Type -> Map Text Type
unifyCandidates candidates splitted = Map.map (remapLabels labelMapping) $ replacements splitted
  where
    unifiedType  :: [Text] -> Type
    unifiedType cset      = foldr1 unifyTypes         $ 
                            map (splitted Map.!) cset
    replace      :: [Text] -> Map Text Type -> Map Text Type
    replace  cset@(c:_) s = Map.insert c (unifiedType cset) (foldr Map.delete s cset)
    replacements :: Map Text Type -> Map Text Type
    replacements        s = foldr replace s candidates
    labelMapping :: Map Text Text
    labelMapping          = Map.fromList $ concatMap mapEntry candidates
    mapEntry cset@(c:_)   = [(x, c) | x <- cset]

-- | Remaps type labels according to a `Map`.
remapLabels :: Map Text Text -> Type -> Type
remapLabels ls (TObj   o) = TObj   $ Dict $ Map.map (remapLabels ls) $ unDict o
remapLabels ls (TArray t) = TArray $                 remapLabels ls  t
remapLabels ls (TUnion u) = TUnion $        Set.map (remapLabels ls) u
remapLabels ls (TLabel l) = TLabel $ Map.lookupDefault l l ls
remapLabels ls other      = other

