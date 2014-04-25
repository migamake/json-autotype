{-# LANGUAGE DeriveDataTypeable #-}
module Data.Aeson.AutoType.Extract(valueSize, typeSize, valueTypeSize,
                                   valueDepth, Dict(..),
                                   Type(..), emptyType,
                                   extractType, unifyTypes) where

import           Control.Exception  (assert)
import           Data.Aeson.AutoType.Type
import           Control.Lens.TH
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict as Hash
import qualified Data.Set            as Set
import qualified Data.Vector         as V
import           Data.Data          (Data(..))
import           Data.Typeable      (Typeable(..))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Text          (Text)
import           Data.Set           (Set )
import           Data.HashMap.Strict(HashMap)
import           Data.List          (sort, foldl1')
import           Data.Ord           (Ord(..), comparing)

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

extractType (Object o)                   = TObj $ Dict $ Hash.map extractType o
extractType  Null                        = TNull
extractType (Bool   b)                   = TBool
extractType (Number n)                   = TNum
extractType (String s)                   = TString
extractType (Array  a) | V.null a        = TArray emptyType
extractType (Array  a)                   = V.foldl1' unifyTypes $ V.map extractType a

simplifyUnion (TUnion s) | Set.size s == 1 = head $ Set.toList s
simplifyUnion t                            = t

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

unifyUnion u v = assertions $
                   union $ uSimple `Set.union`
                           vSimple `Set.union`
                           oset
  where
    (uSimple, uCompound) = Set.partition isSimple u
    (vSimple, vCompound) = Set.partition isSimple v
    assertions = assert (Set.null $ Set.filter (not . isArray) uArr) .
                 assert (Set.null $ Set.filter (not . isArray) vArr) 
    (uObj, uArr) = Set.partition isObject uCompound
    (vObj, vArr) = Set.partition isObject vCompound
    oset    = Set.fromList $ if null objects
                               then []
                               else [foldl1' unifyTypes objects]
    aset    = Set.fromList $ if null arrays
                               then []
                               else [foldl1' unifyTypes arrays]
    objects = Set.toList $ uObj `Set.union` vObj
    arrays  = Set.toList $ uArr `Set.union` vArr

union = simplifyUnion . TUnion

