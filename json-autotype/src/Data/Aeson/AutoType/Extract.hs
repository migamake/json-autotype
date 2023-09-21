-- | Extraction and unification of AutoType's @Type@ from Aeson @Value@.
module Data.Aeson.AutoType.Extract(valueSize, valueTypeSize,
                                   valueDepth, Dict(..),
                                   Type(..), emptyType,
                                   extractType, unifyTypes,
                                   typeCheck) where

import           Control.Arrow ((&&&))
import           Control.Exception               (assert)
import           Data.Aeson.Key                  (toText)
import           Data.Aeson.KeyMap               (toHashMap)
import           Data.Aeson.AutoType.Type
import qualified Data.Graph          as Graph
import qualified Data.HashMap.Strict      as Map
import           Data.HashMap.Strict             (HashMap, mapKeys)
import qualified Data.Set                 as Set
import qualified Data.Vector              as V
import           Data.Aeson
import           Data.Text                       (Text)
import           Data.Set                        (Set )
import           Data.List                       (foldl1')
import           Data.Scientific                 (isInteger)

--import           Debug.Trace

-- Convert from Aeson's @KeyMap v@ type to Autotype's @HashMap Text v@ type.
toHashMapTxt = mapKeys toText . toHashMap

-- | Compute total number of nodes (and leaves) within the value tree.
-- Each simple JavaScript type (including String) is counted as of size 1,
-- whereas both Array or object types are counted as 1+sum of the sizes
-- of their member values.
valueSize :: Value -> Int
valueSize  Null      = 1
valueSize (Bool   _) = 1
valueSize (Number _) = 1
valueSize (String _) = 1
valueSize (Array  a) = V.foldl' (+) 1 $ V.map valueSize a
valueSize (Object o) = (1+) . sum . map valueSize . Map.elems $ toHashMapTxt o

-- | Compute total size of the type of the @Value@.
-- For:
-- * simple types it is always 1,
-- * for arrays it is just 1+_maximum_ size of the (single) element type,
-- * for objects it is _sum_ of the sizes of fields (since each field type
--   is assumed to be different.)
valueTypeSize :: Value -> Int
valueTypeSize  Null      = 1
valueTypeSize (Bool   _) = 1
valueTypeSize (Number _) = 1
valueTypeSize (String _) = 1
valueTypeSize (Array  a) = (1+) . V.foldl' max 0 $ V.map valueTypeSize a
valueTypeSize (Object o) = (1+) . sum . map valueTypeSize . Map.elems $ toHashMapTxt o

-- | Compute total depth of the value.
-- For:
-- * simple types it is 1
-- * for either Array or Object, it is 1 + maximum of depths of their members
valueDepth :: Value -> Int
valueDepth  Null      = 1
valueDepth (Bool   _) = 1
valueDepth (Number _) = 1
valueDepth (String _) = 1
valueDepth (Array  a) = (1+) . V.foldl' max 0 $ V.map valueDepth a
valueDepth (Object o) = (1+) . maximum . (0:) . map valueDepth . Map.elems $ toHashMapTxt o

-- | Check if a number is integral, or floating point
-- | Extract @Type@ from the JSON @Value@.
-- Unifying types of array elements, if necessary.
extractType                            :: Value -> Type
extractType (Object o)                  = TObj $ Dict $ Map.map extractType $ toHashMapTxt o
extractType  Null                       = TNull
extractType (Bool   _)                  = TBool
extractType (Number n) | isInteger n    = TInt
extractType (Number _)                  = TDouble
extractType (String _)                  = TString
extractType (Array  a) | V.null a       = TArray   emptyType
extractType (Array  a)                  = TArray $ V.foldl1' unifyTypes $ traceShow $ V.map extractType a
  where
    --traceShow a = trace (show a) a
    traceShow = id

-- | Type check the value with the derived type.
typeCheck :: Value -> Type -> Bool
typeCheck  Null          TNull            = True
typeCheck  v            (TUnion  u)       = typeCheck v `any` Set.toList u
typeCheck (Bool   _)     TBool            = True
typeCheck (String _)     TString          = True
typeCheck (Number n)     TInt             = isInteger n
typeCheck (Number _)     TDouble          = True
typeCheck (Array  elts) (TArray  eltType) = (`typeCheck` eltType) `all` V.toList elts
typeCheck (Object d)    (TObj    e      ) = typeCheckKey `all` keysOfBoth
  where
    typeCheckKey k = getValue k (toHashMapTxt d) `typeCheck` get k e
    getValue   :: Text -> HashMap Text Value -> Value
    getValue    = Map.lookupDefault Null
    keysOfBoth :: [Text]
    keysOfBoth  =  Set.toList $ Set.fromList (Map.keys $ toHashMapTxt d) `Set.union` keys e
typeCheck         _     (TLabel  _      ) = error "Cannot typecheck labels without environment!"
typeCheck   {-a-} _      _ {-b-}          = {-trace msg $-} False
  where
    -- msg = "Mismatch: " ++ show a ++ " :: " ++ show b

allKeys :: Dict -> Dict -> [Text]
d `allKeys` e = Set.toList (keys d `Set.union` keys e)

-- | Standard unification procedure on @Type@s,
-- with inclusion of @Type@ unions.
unifyTypes :: Type -> Type -> Type
unifyTypes  TBool      TBool     = TBool
unifyTypes  TInt       TInt      = TInt
unifyTypes  TDouble    TInt      = TDouble
unifyTypes  TInt       TDouble   = TDouble
unifyTypes  TDouble    TDouble   = TDouble
unifyTypes  TString    TString   = TString
unifyTypes  TNull      TNull     = TNull
unifyTypes (TObj   d) (TObj   e) = TObj newDict
  where
    newDict :: Dict
    newDict = Dict $ Map.fromList [(k, get k d `unifyTypes`
                                        get k e) | k <- allKeys d e ]
unifyTypes (TArray u) (TArray v) = TArray $ u `unifyTypes` v
unifyTypes t           s         = typeAsSet t `unifyUnion` typeAsSet s

-- | Unify sets of types (sets are union types of alternatives).
unifyUnion :: Set Type -> Set Type -> Type
unifyUnion u v = assertions $
                   union $ uSimple        `Set.union`
                           vSimple        `Set.union`
                           unifiedObjects `Set.union`
                           Set.singleton unifiedArray
  where
    -- We partition our types for easier unification into simple and compound
    (uSimple, uCompound) = Set.partition isSimple u
    (vSimple, vCompound) = Set.partition isSimple v
    assertions = assert (Set.null $ Set.filter (not . isArray) uArr) .
                 assert (Set.null $ Set.filter (not . isArray) vArr)
    -- then we partition compound typs into objects and arrays.
    -- Note that there should be no TUnion here, since we are inside a TUnion already.
    -- (That is reduced by @union@ smart costructor as superfluous.)
    (uObj, uArr)   = Set.partition isObject uCompound
    (vObj, vArr)   = Set.partition isObject vCompound
    unifiedObjects = Set.fromList $ if null objects
                                       then []
                                       else [foldl1' unifyTypes objects]
    objects = Set.toList $ uObj `Set.union` vObj
    arrayElts :: [Type]
    arrayElts  = map (\(TArray ty) -> ty) $
                   Set.toList $
                     uArr `Set.union` vArr
    unifiedArray = TArray $ if null arrayElts
                               then emptyType
                               else foldl1' unifyTypes arrayElts

-- | Smart constructor for union types.
union ::  Set Type -> Type
union = simplifyUnion . TUnion

-- | Simplify TUnion's so there is no TUnion directly inside TUnion.
-- If there is only one element of the set, then return this single
-- element as a type.
simplifyUnion :: Type -> Type
simplifyUnion (TUnion s) | Set.size s == 1 = head $ Set.toList s
simplifyUnion (TUnion s)                   = TUnion $ Set.unions $ map elements $ Set.toList s
  where
    elements (TUnion elems) = elems
    elements sing           = Set.singleton sing
simplifyUnion unexpected                   = error ("simplifyUnion: unexpected argument " ++ show unexpected)

