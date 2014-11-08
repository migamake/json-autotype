module Data.Aeson.AutoType.Util( withFileOrHandle
                               , withFileOrDefaultHandle
                               , assertM ) where

import           Data.Hashable
import qualified Data.Set as Set
import           Control.Exception(assert)
import           System.IO                 (withFile, IOMode(..), Handle, stdin, stdout)

-- | Generic function for opening file if the filename is not empty nor "-",
--   or using given handle otherwise (probably stdout, stderr, or stdin).
-- TODO: Should it become utility function?
withFileOrHandle :: FilePath -> IOMode -> Handle -> (Handle -> IO r) -> IO r
withFileOrHandle        ""       _         handle action =                      action handle
withFileOrHandle        "-"      _         handle action =                      action handle
withFileOrHandle        name     ioMode    _      action = withFile name ioMode action 

withFileOrDefaultHandle :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFileOrDefaultHandle "-"      ReadMode         action = action stdin
withFileOrDefaultHandle "-"      WriteMode        action = action stdout
withFileOrDefaultHandle "-"      otherMode        _      = error $ "Incompatible io mode ("
                                                                ++ show otherMode
                                                                ++ ") for `-` in withFileOrDefaultHandle." 
withFileOrDefaultHandle filename ioMode           action = withFile filename ioMode action


assertM ::  Monad m => Bool -> m ()
assertM v = assert v $ return ()


-- Missing instances
instance Hashable a => Hashable (Set.Set a) where
  hashWithSalt = Set.foldr (flip hashWithSalt)

 
 
