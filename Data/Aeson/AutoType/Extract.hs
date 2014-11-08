-- | Extraction and unification of AutoType's @Type@ from Aeson @Value@.
module Data.Aeson.AutoType.Extract(valueSize, typeSize, valueTypeSize,
                                   valueDepth, Dict(..),
                                   Type(..), emptyType,
                                   extractType, unifyTypes) where

import           Control.Exception  (assert)
import           Data.Aeson.AutoType.Type
import qualified Data.HashMap.Strict as Hash
import qualified Data.Set            as Set
import qualified Data.Vector         as V
--import           Data.Typeable      (Typeable)
import           Data.Aeson
import           Data.Text          (Text)
import           Data.Set           (Set )
import           Data.List          (foldl1')

valueSize :: Value -> Int
valueSize  Null      = 1
valueSize (Bool   _) = 1
valueSize (Number _) = 1
valueSize (String _) = 1
valueSize (Array  a) = V.foldl' (+) 1 $ V.map valueSize a
valueSize (Object o) = (1+) . sum . map valueSize . Hash.elems $ o

valueTypeSize :: Value -> Int
valueTypeSize  Null      = 1
valueTypeSize (Bool   _) = 1
valueTypeSize (Number _) = 1
valueTypeSize (String _) = 1
valueTypeSize (Array  a) = (1+) . V.foldl' max 0 $ V.map valueTypeSize a
valueTypeSize (Object o) = (1+) . sum . map valueTypeSize . Hash.elems $ o

valueDepth :: Value -> Int
valueDepth  Null      = 1
valueDepth (Bool   _) = 1
valueDepth (Number _) = 1
valueDepth (String _) = 1
valueDepth (Array  a) = (1+) . V.foldl' max 0 $ V.map valueDepth a
valueDepth (Object o) = (1+) . maximum . (0:) . map valueDepth . Hash.elems $ o

extractType :: Value -> Type
extractType (Object o)                   = TObj $ Dict $ Hash.map extractType o
extractType  Null                        = TNull
extractType (Bool   _)                   = TBool
extractType (Number _)                   = TNum
extractType (String _)                   = TString
extractType (Array  a) | V.null a        = TArray emptyType
extractType (Array  a)                   = TArray $ V.foldl1' unifyTypes $ V.map extractType a

unifyTypes :: Type -> Type -> Type
unifyTypes TBool        TBool                         = TBool
unifyTypes TNum         TNum                          = TNum
unifyTypes TString      TString                       = TString
unifyTypes TNull        TNull                         = TNull
unifyTypes (TObj d)     (TObj  e)                     = TObj newDict 
  where
    newDict :: Dict
    newDict = Dict $ Hash.fromList [(k, get k d `unifyTypes`
                                        get k e) | k <- allKeys ]
    allKeys :: [Text]
    allKeys = Set.toList (keys d `Set.union` keys e)
unifyTypes (TArray u)   (TArray v)                    = TArray $ u `unifyTypes` v
unifyTypes t            s                             = typeAsSet t `unifyUnion` typeAsSet s

-- | Unify sets of types (sets are union types of alternatives).
unifyUnion :: Set Type -> Set Type -> Type
unifyUnion u v = assertions $
                   union $ uSimple `Set.union`
                           vSimple `Set.union`
                           oset
  where
    -- We partition our types for easier unification into simple and compound
    (uSimple, uCompound) = Set.partition isSimple u
    (vSimple, vCompound) = Set.partition isSimple v
    assertions = assert (Set.null $ Set.filter (not . isArray) uArr) .
                 assert (Set.null $ Set.filter (not . isArray) vArr)
    -- then we partition compound typs into objects and arrays.
    -- Note that there should be no TUnion here, since we are inside a TUnion already.
    -- (That is reduced by @union@ smart costructor as superfluous.)
    (uObj, uArr) = Set.partition isObject uCompound
    (vObj, vArr) = Set.partition isObject vCompound
    oset    = Set.fromList $ if null objects
                               then []
                               else [foldl1' unifyTypes objects]
    objects = Set.toList $ uObj `Set.union` vObj
    {-aset    = Set.fromList $ if null arrays
                               then []
                               else [foldl1' unifyTypes arrays]-}
    --arrays  = Set.toList $ uArr `Set.union` vArr

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
    elements s              = Set.singleton s
