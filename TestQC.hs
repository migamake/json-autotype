{-# LANGUAGE TemplateHaskell #-}
module Main(
    main
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Test() -- Arbitrary instance for Value

import           Test.QuickCheck
import           Test.SmallCheck
--import           Test.QuickCheck.Arbitrary

prop_typeCheck  ::  Value -> Bool
prop_typeCheck v = v `typeCheck` extractType v

{-
prop_typeSize  ::  Value -> Bool
prop_typeSize v = valueSize v >= typeSize (extractType v)

prop_valueAndValueTypeSize  ::  Value -> Bool
prop_valueAndValueTypeSize v = valueSize v >= valueTypeSize v

prop_valueTypeSizeAndTypeSize  ::  Value -> Bool
prop_valueTypeSizeAndTypeSize v = valueTypeSize v >= typeSize (extractType v) -}

-- | Maximum reasonable depth for quick testing
depth = 5

main :: IO ()
main  = do smallCheck depth prop_typeCheck
           quickCheckWith myArgs prop_typeCheck
                      {- prop_typeSize,
                      prop_valueAndValueTypeSize,
                      prop_valueTypeSizeAndTypeSize]-}
  where
    -- 17 - reasonable size for runghc
    --myArgs i = stdArgs { maxSize=i }
    myArgs = stdArgs { maxSize=17, maxSuccess=1000 }
