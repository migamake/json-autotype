{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Union types describing JSON objects, and operations for querying these types.
module Data.Aeson.AutoType.Type(typeSize,
                                Dict(..), keys, get, withDict,
                                Type(..), emptyType,
                                isSimple, isArray, isObject, typeAsSet,
                                hasNonTopTObj,
                                hasTObj,
                                isNullable,
                                emptySetLikes
  ) where

import           Prelude             hiding (any)
import qualified Data.HashMap.Strict as Hash
import qualified Data.Set            as Set
import           Data.Data          (Data(..))
import           Data.Typeable      (Typeable)
import           Data.Foldable      (any)
import           Data.Text          (Text)
import           Data.Set           (Set )
import           Data.HashMap.Strict(HashMap)
import           Data.List          (sort)
import           Data.Ord           (comparing)
import           Data.Generics.Uniplate
import           GHC.Generics      (Generic)

-- * Dictionary types for overloading of usual class instances.
-- | Type alias for HashMap
type Map = HashMap

-- | Dictionary of types indexed by names.
newtype Dict = Dict { unDict :: Map Text Type }
  deriving (Eq, Data, Typeable, Generic)

instance Show Dict where
  show = show . sort . Hash.toList . unDict

instance Ord Dict where
  compare = comparing $ sort . Hash.toList . unDict

-- | Make operation on a map to an operation on a Dict.
withDict :: (Map Text Type -> Map Text Type) -> Dict -> Dict
f `withDict` (Dict m) = Dict $ f m

-- | Take all keys from dictionary.
keys :: Dict -> Set Text
keys = Set.fromList . Hash.keys . unDict

-- | Union types for JSON values.
data Type = TNull | TBool | TString        |
            TInt  | TDouble                |
            TUnion (Set      Type)         |
            TLabel  Text                   |
            TObj    Dict                   |
            TArray  Type
  deriving (Show,Eq, Ord, Data, Typeable, Generic)

-- These are missing Uniplate instances...
{-
instance Biplate (Set a) a where
  biplate s = (Set.toList s, Set.fromList)

instance Biplate (HashMap k v) v where
  biplate m = (Hash.elems m, Hash.fromList . zip (Hash.keys m))
 -}

instance Uniplate Type where
  uniplate (TUnion s) = (Set.toList s, TUnion .        Set.fromList                     )
  uniplate (TObj   d) = (Hash.elems m, TObj   . Dict . Hash.fromList . zip (Hash.keys m))
    where
      m = unDict d
  uniplate (TArray t) = ([t],          TArray . head  )
  uniplate s          = ([],           const s        )

-- | Empty type
emptyType :: Type
emptyType = TUnion Set.empty

-- | Lookup the Type within the dictionary.
get :: Text -> Dict -> Type
get key = Hash.lookupDefault TNull key . unDict

-- $derive makeUniplateDirect ''Type

-- | Size of the `Type` term.
typeSize           :: Type -> Int
typeSize TNull      = 1
typeSize TBool      = 1
typeSize TString    = 1
typeSize TInt       = 1
typeSize TDouble    = 1
typeSize (TObj   o) = (1+) . sum     . map typeSize . Hash.elems . unDict $ o
typeSize (TArray a) = 1 + typeSize a
typeSize (TUnion u) = (1+) . sum . (0:) . map typeSize . Set.toList $ u
typeSize (TLabel _) = error "Don't know how to compute typeSize of TLabel."

-- | Check if this is nullable (Maybe) type, or not.
-- Nullable type will always accept TNull or missing key that contains it.
isNullable :: Type -> Bool
isNullable  TNull     = True
isNullable (TUnion u) = isNullable `any` u
isNullable  _         = False

-- | "Null-ish" types
emptySetLikes ::  Set Type
emptySetLikes = Set.fromList [TNull, TArray $ TUnion $ Set.fromList []]
-- Q: and TObj $ Map.fromList []?
{-# INLINE emptySetLikes #-}

-- | Convert any type into union type (even if just singleton).
typeAsSet :: Type -> Set Type
typeAsSet (TUnion s) = s
typeAsSet t          = Set.singleton t

-- | Is the top-level constructor a TObj?
isObject         :: Type -> Bool
isObject (TObj _) = True
isObject _        = False

-- | Is it a simple (non-compound) Type?
isSimple  :: Type -> Bool
isSimple x = not (isObject x) && not (isArray x) && not (isUnion x)

-- | Is the top-level constructor a TUnion?
isUnion           :: Type -> Bool
isUnion (TUnion _) = True
isUnion _          = False

-- | Is the top-level constructor a TArray?
-- | Check if the given type has non-top TObj.
isArray           :: Type -> Bool
isArray (TArray _) = True
isArray _          = False

-- | Check if the given type has non-top TObj.
hasNonTopTObj         :: Type -> Bool
hasNonTopTObj (TObj o) = any hasTObj $ Hash.elems $ unDict o
hasNonTopTObj _        = False

-- | Check if the given type has TObj on top or within array..
hasTObj           :: Type -> Bool
hasTObj (TObj   _) = True
hasTObj (TArray a) = hasTObj a
hasTObj (TUnion u) = any hasTObj u
hasTObj _          = False
