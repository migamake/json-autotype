{-# LANGUAGE TemplateHaskell #-}
module Data.Aeson.AutoType.Extract(valueSize, typeSize, valueTypeSize,
                                   valueDepth, Dict(..),
                                   Type(..), emptyType,
                                   extractType, unifyTypes) where

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

type Map = HashMap

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

-- * Dictionary of types indexed by names.
newtype Dict = Dict { unDict :: Map Text Type }
  deriving (Show, Eq)

instance Ord Dict where
  compare = comparing $ sort . Hash.toList . unDict

-- | Take all keys from dictionary.
keys :: Dict -> Set Text
keys = Set.fromList . Hash.keys . unDict

-- | Lookup the type within the dictionary.
get :: Text -> Dict -> Type
get key = Hash.lookupDefault emptyType key . unDict 

data Type = TNull | TBool | TNum | TString |
            TUnion (Set.Set      Type)     |
            TLabel Text                    |
            TObj   Dict                    |
            TArray Type
  deriving (Show,Eq, Ord)

typeSize TNull      = 1
typeSize TBool      = 1
typeSize TNum       = 1
typeSize TString    = 1
typeSize (TObj   o) = (1+) . sum     . map typeSize . Hash.elems . unDict $ o
typeSize (TArray a) = 1 + typeSize a
typeSize (TUnion u) = (1+) . maximum . (0:) . map typeSize . Set.toList $ u

-- | Empty type
emptyType :: Type
emptyType = TUnion Set.empty 

extractType (Object o)                   = TObj $ Dict $ Hash.map extractType o
extractType  Null                        = TNull
extractType (Bool   b)                   = TBool
extractType (Number n)                   = TNum
extractType (String s)                   = TString
extractType (Array  a) | V.null a        = TArray $ emptyType
extractType (Array  a)                   = V.foldl1' unifyTypes $ V.map extractType a

simplifyUnion (TUnion s) | Set.size s == 1 = head $ Set.toList s
simplifyUnion t                            = t

mkUnionType t@(TUnion _) = t
mkUnionType t            = TUnion $ Set.singleton t

isObject (TObj _) = False
isObject _        = True

unifyTypes TBool        TBool                         = TBool
unifyTypes TNum         TNum                          = TNum
unifyTypes TString      TString                       = TString
unifyTypes TNull        TNull                         = TNull
unifyTypes (TObj d)     (TObj  e)                     = TObj $ newDict 
  where
    newDict :: Dict
    newDict = Dict $ Hash.fromList [(k, get k d `unifyTypes`
                                        get k e) | k <- allKeys ]
    allKeys :: [Text]
    allKeys = Set.toList (keys d `Set.union` keys e)
unifyTypes (TUnion u)   (TUnion v)                    = union $ uSimple `Set.union`
                                                                vSimple `Set.union`
                                                                oset
  where
    (uObj, uSimple) = Set.partition isObject u
    (vObj, vSimple) = Set.partition isObject v
    oset    = Set.fromList $ if objects == []
                               then []
                               else [foldl1' unifyTypes objects]
    objects = Set.toList uObj ++ Set.toList vObj
unifyTypes u@(TUnion _) t                             = u `unifyTypes` mkUnionType t
unifyTypes t            u@(TUnion _)                  = u `unifyTypes` mkUnionType t
unifyTypes t            r                             = union $ Set.fromList [t, r]

union = simplifyUnion . TUnion

