-- Test over all files in examples/ directory
module Main(main) where

import Control.Monad(forM, forM_, unless, join)
import Data.Char(toUpper)
import Data.Functor ((<$>))
import Data.List(isPrefixOf, isSuffixOf)
import System.Directory(doesDirectoryExist, getDirectoryContents)
import System.FilePath((</>), (<.>), takeBaseName, replaceFileName)
import System.Exit(ExitCode(..))
import Data.Aeson.AutoType.CodeGen(runModule, Lang(Haskell))
import Data.Aeson ( Result,  Object, FromJSON, Value(Null,Number), (.:?) )
import Data.Aeson.Types ( Parser, parse )
import Data.Text ( Text, pack )
import Data.HashMap.Lazy ( singleton, empty )

--import CommonCLI

runghc :: [String] -> IO ExitCode
runghc = runModule Haskell
-- runModule HaskellStrict -- for compiling with -Wall -Werror

-- |  <http://book.realworldhaskell.org/read/io-case-study-a-library-for-searching-the-filesystem.html>
getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  ex<-doesDirectoryExist topdir
  if ex
        then do
          names <- getDirectoryContents topdir
          let properNames = filter (not . isPrefixOf ".") names
          paths <- forM properNames $ \name -> do
            let path = topdir </> name
            isDirectory <- doesDirectoryExist path
            if isDirectory
              then getRecursiveContents path
              else return [path]
          return (concat paths)
        else return []

capitalize :: String -> String
capitalize [] = []
capitalize (s:ss) = toUpper s:ss

main :: IO ()
main  = do
  verifyAesonOperators
  filenames <-  filter (isSuffixOf ".json")
            <$> getRecursiveContents "examples"
  forM_ filenames $ \filename -> do
    let outputFilename = filename `replaceFileName` capitalize (takeBaseName filename <.> "hs")
    genResult <- runghc ["GenerateJSONParser.hs", filename, "--outputFilename", outputFilename]
    unless (genResult == ExitSuccess) $
      fail (unwords ["test case", show filename, "failed with", show genResult])
    parserResult <- runghc [outputFilename, filename]
    unless (parserResult == ExitSuccess) $
      fail (unwords ["generated parser", show outputFilename, "failed with", show parserResult])

-- Verify that aeson's (.:?) operator has the same semantics as our (.:??).
-- See https://github.com/bos/aeson/issues/287 for details.

verifyAesonOperators :: IO ()
verifyAesonOperators = do
  parseTest (singleton (pack "foo") (Number 1))
  parseTest (singleton (pack "foo") Null)
  parseTest (singleton (pack "bar") Null)
  parseTest empty

(.:??) :: FromJSON a => Object -> Text -> Parser (Maybe a)
o .:?? val = fmap join (o .:? val)

parseTest :: Object -> IO ()
parseTest o = unless (r1 == r2) (fail (show r1 ++ " /= " ++ show r2))
  where r1, r2 :: Result (Maybe Int)
        r1 = parse (.:? (pack "foo")) o
        r2 = parse (.:?? (pack "foo")) o
