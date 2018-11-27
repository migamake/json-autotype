module Data.Aeson.AutoType.CodeGen.Common(writeRunningCommandComment) where

import System.Environment(getArgs, getProgName)
import System.IO

-- | Write a comment with urrently running command (for documentation of generated code.)
writeRunningCommandComment :: Handle -> String -> IO ()
writeRunningCommandComment outHandle commentString = do
  prog <- getProgName
  args <- getArgs
  hPutStrLn outHandle $ unwords $ commentString:prog:args


