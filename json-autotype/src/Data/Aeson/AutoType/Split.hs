{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGuaGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGuaGE DeriveGeneric       #-}
{-# LANGuaGE FlexibleContexts    #-}
-- | Formatting type declarations and class instances for inferred types. 
module Data.Aeson.AutoType.Split(
  splitTypeByLabel, unificationCandidates,
  unifyCandidates, toposort
) where

import           Control.Arrow             ((&&&))
import           Control.Applicative       ((<$>), (<*>))
import           Control.Lens.TH
import           Control.Lens
import           Control.Monad             (forM)
import           Control.Exception(assert)
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import qualified Data.Set                   as Set
import qualified Data.Text                  as Text
import           Data.Text                 (Text)
import           Data.Set                  (Set )
import           Data.List                 (foldl1')
import           Data.Char                 (isAlpha, isDigit)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict(State, runState)
import qualified Data.Graph          as Graph
import           GHC.Generics              (Generic)

import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Util  ()

--import           Debug.Trace -- DEBUG
trace _ x = x

fst3 ::  (t, t1, t2) -> t
fst3 (a, _b, _c) = a

type Map k v = Map.HashMap k v 

-- | Explanatory type alias for making declarations
-- First element of the triple is original JSON identifier,
-- second element of the triple is the mapped identifier name in Haskell.
-- third element of the triple shows the type in a formatted way
type MappedKey = (Text, Text, Text, Bool)

-- * Splitting object types by label for unification.
type TypeTree    = Map Text [Type]

type TypeTreeM a = State TypeTree a

addType :: Text -> Type -> TypeTreeM ()
addType label typ = modify $ Map.insertWith (++) label [typ]

splitTypeByLabel' :: Text -> Type -> TypeTreeM Type
splitTypeByLabel' _  TString   = return TString
splitTypeByLabel' _  TInt      = return TInt
splitTypeByLabel' _  TDouble   = return TDouble
splitTypeByLabel' _  TBool     = return TBool
splitTypeByLabel' _  TNull     = return TNull
splitTypeByLabel' _ (TLabel r) = error $ "Splitting into labelled types after label "
                                      <> show r <> " was already given!"
splitTypeByLabel' l (TUnion u) = do m <- mapM (splitTypeByLabel' l) $ Set.toList u
                                    return $! TUnion $! Set.fromList m
splitTypeByLabel' l (TArray a) = do m <- splitTypeByLabel' (l `Text.append` "Elt") a
                                    return $! TArray m
splitTypeByLabel' l (TObj   o) = do kvs <- forM (Map.toList $ unDict o) $ \(k, v) -> do
                                       component <- splitTypeByLabel' k v
                                       return (k, component)
                                    addType l (TObj $ Dict $ Map.fromList kvs)
                                    return $! TLabel l

-- | Splits initial type with a given label, into a mapping of object type names and object type structures.
splitTypeByLabel :: Text -> Type -> Map Text Type
splitTypeByLabel topLabel t = Map.map (foldl1' unifyTypes) finalState
  where
    finalize (TLabel l) = assert (l == topLabel) $ return ()
    finalize  topLevel  = addType topLabel topLevel
    initialState    = Map.empty
    (_, finalState) = runState (splitTypeByLabel' topLabel t >>= finalize) initialState

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
    go _other     ls = ls

-- * Finding candidates for extra unifications
-- | For a given splitted types, it returns candidates for extra
-- unifications.
unificationCandidates :: Map.HashMap t Type -> [[t]]
unificationCandidates = Map.elems             .
                        Map.filter candidates .
                        Map.fromListWith (++) .
                        concatMap entry       .
                        Map.toList
  where
    -- | Candidate entry has to have at least two candidates, so that unification makes sense
    candidates [ ] = False
    candidates [_] = False
    candidates _   = True
    -- | Make a candidate entry for each object type, which points from its keys to its label.
    entry (k, TObj o)                 = [(Set.fromList $ Map.keys $ unDict o, [k])]
    entry  _                          = [] -- ignore array elements and toplevel type if it is Array

-- | Unifies candidates on a give input list.
unifyCandidates :: [[Text]] -> Map Text Type -> Map Text Type
unifyCandidates candidates splitted = Map.map (remapLabels labelMapping) $ replacements splitted
  where
    unifiedType  :: [Text] -> Type
    unifiedType cset      = foldr1 unifyTypes         $ 
                            map (splitted Map.!) cset
    replace      :: [Text] -> Map Text Type -> Map Text Type
    replace  cset@(c:_) s = Map.insert c (unifiedType cset) (foldr Map.delete s cset)
    replace  []         _ = error "Empty candidate set in replace"
    replacements :: Map Text Type -> Map Text Type
    replacements        s = foldr replace s candidates
    labelMapping :: Map Text Text
    labelMapping          = Map.fromList $ concatMap mapEntry candidates
    mapEntry cset@(c:_)   = [(x, c) | x <- cset]
    mapEntry []           = error "Empty candidate set in mapEntry"

-- | Remaps type labels according to a `Map`.
remapLabels :: Map Text Text -> Type -> Type
remapLabels ls (TObj   o) = TObj   $ Dict $ Map.map (remapLabels ls) $ unDict o
remapLabels ls (TArray t) = TArray $                 remapLabels ls  t
remapLabels ls (TUnion u) = TUnion $        Set.map (remapLabels ls) u
remapLabels ls (TLabel l) = TLabel $ Map.lookupDefault l l ls
remapLabels _  other      = other

