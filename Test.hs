{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           System.IO                 (withFile, stderr, stdout, IOMode(WriteMode), Handle)
import           System.FilePath           (FilePath, splitExtension)
import           System.Environment        (getArgs)
import           Control.Arrow             ((&&&))
import           Control.Lens.TH
import           Control.Lens
import           Control.Monad             (forM, forM_, when)
import           Control.Exception(assert)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.HashMap.Strict        as Map
import qualified Data.Set                   as Set
import qualified Data.Vector                as V
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Data.Text                 (Text)
import           Data.Set                  (Set )
import           Data.List                 (sort, foldl1')
import           Data.Ord                  (Ord(..), comparing)
import           Data.Char                 (isAlpha)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict(State, runState)
import           Data.Hashable             (Hashable(..))
import qualified Data.Graph          as Graph

import           Data.Aeson.AutoType.Type
import           Data.Aeson.AutoType.Extract
import           Data.Aeson.AutoType.Util
import           Data.Aeson.AutoType.Format
import           HFlags

--import           Data.Tuple.Utils          (fst3)
fst3 (a, _, _) = a

assertM v = assert v $ return ()

header moduleName = Text.unlines ["{-# LANGUAGE TemplateHaskell #-}"
                      ,Text.concat ["module ", moduleName, " where"]
                      ,""
                      ,"import           Data.Text (Text)"
                      ,"import           Data.Aeson(decode, Value(..), FromJSON(..),"
                      ,"                            (.:), (.:?), (.!=))"
                      ,"import           Data.Aeson.TH"
                      ,""]

-- * Command line flags
defineFlag "filename"  ("JSONTypes.hs" :: FilePath) "Write output to the given file"
defineFlag "suggest"   True                         "Suggest candidates for unification"
defineFlag "autounify" True                         "Automatically unify suggested candidates"
defineFlag "fakeFlag"  True                         "Ignore this flag - it doesn't exist!!!"

-- | Generic function for opening file if the filename is not empty nor "-",
--   or using given handle otherwise (probably stdout, stderr, or stdin).
-- TODO: Should it become utility function?
withFileOrHandle :: FilePath -> IOMode -> Handle -> (Handle -> IO r) -> IO r
withFileOrHandle ""   ioMode handle action =                      action handle
withFileOrHandle "-"  ioMode handle action =                      action handle
withFileOrHandle name ioMode _      action = withFile name ioMode action 

main = do $initHFlags "json-autotype -- automatic type and parser generation from JSON"
          filenames <- getArgs
          let (moduleName, extension) = splitExtension flags_filename
          assertM $ extension == ".hs"
          -- TODO: should integrate all inputs into single type set!!!
          withFileOrHandle flags_filename WriteMode stdout $ \hOut ->
            forM filenames $ \filename ->
              do bs <- BSL.readFile filename
                 Text.hPutStrLn stderr $ Text.pack $ show moduleName
                 let Just v   = decode bs
                 let t        = extractType v
                 let splitted = splitTypeByLabel "TopLevel" t
                 Text.hPutStrLn hOut $ Text.pack moduleName
                 Text.hPutStrLn hOut $ header $ Text.pack moduleName
                 assertM $ not $ any hasNonTopTObj $ Map.elems splitted
                 let uCands = unificationCandidates splitted
                 when flags_suggest $ forM_ uCands $ \cs -> do
                                        putStr "-- "
                                        Text.putStrLn $ "=" `Text.intercalate` cs
                 let unified = if flags_autounify
                                 then unifyCandidates uCands splitted
                                 else splitted
                 Text.hPutStrLn hOut $ displaySplitTypes unified

