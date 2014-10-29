-- | This module facilitates definition of command line interface.
module CLI (
    defaultOutputFilename
  , withFileOrHandle
  ) where

import           System.IO                 (withFile, IOMode(..), Handle)

-- | Default output filname is used, when there is no explicit output file path, or it is "-" (stdout).
defaultOutputFilename :: FilePath
defaultOutputFilename = "JSONTypes.hs"

-- | Generic function for opening file if the filename is not empty nor "-",
--   or using given handle otherwise (probably stdout, stderr, or stdin).
-- TODO: Should it become utility function?
withFileOrHandle :: FilePath -> IOMode -> Handle -> (Handle -> IO r) -> IO r
withFileOrHandle ""   _      handle action =                      action handle
withFileOrHandle "-"  _      handle action =                      action handle
withFileOrHandle name ioMode _      action = withFile name ioMode action 

