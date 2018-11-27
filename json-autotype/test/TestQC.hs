-- | QuickCheck instances for automatically generating JSON inputs,
-- and checking that json-autotype works correctly on these.
module Main(
    main
  ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Test() -- Arbitrary instance for Value

import           Test.QuickCheck
--import           Test.QuickCheck.Parallel
import           Test.SmallCheck
--import           Test.QuickCheck.Arbitrary

prop_typeCheck  ::  Value -> Bool
prop_typeCheck v = v `typeCheck` extractType v

-- | Maximum reasonable depth for quick exhaustive testing
depth = 5

main :: IO ()
main  = do smallCheck     depth  prop_typeCheck
           quickCheckWith myArgs prop_typeCheck
  where
    -- 17 - reasonable size for runghc
    --myArgs i = stdArgs { maxSize=i }
    myArgs = stdArgs { maxSize=17, maxSuccess=1000 }
