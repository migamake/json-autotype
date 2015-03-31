{-# LANGUAGE TemplateHaskell #-}
module TestQC(
    main
  ) where

import           Test.QuickCheck
--import           Test.QuickCheck.Arbitrary
--import           Test.QuickCheck.All

import           Data.Aeson
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Test() -- Arbitrary instance for Value
--import           Data.Aeson.AutoType.Type

prop_typeCheck  ::  Value -> Bool
prop_typeCheck v = v `typeCheck` extractType v

{-
prop_typeSize  ::  Value -> Bool
prop_typeSize v = valueSize v >= typeSize (extractType v)

prop_valueAndValueTypeSize  ::  Value -> Bool
prop_valueAndValueTypeSize v = valueSize v >= valueTypeSize v

prop_valueTypeSizeAndTypeSize  ::  Value -> Bool
prop_valueTypeSizeAndTypeSize v = valueTypeSize v >= typeSize (extractType v) -}

main :: IO ()
main  = quickCheck prop_typeCheck
                         {- prop_typeSize,
                          prop_valueAndValueTypeSize,
                          prop_valueTypeSizeAndTypeSize]-}
