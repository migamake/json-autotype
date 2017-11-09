-- Test over all files in examples/ directory
module Main(main) where

import Control.Monad(forM)
import Data.Char(toUpper)
import Data.Functor ((<$>))
import Data.List(isPrefixOf, isSuffixOf)
import System.Directory(doesDirectoryExist, getDirectoryContents)
import System.FilePath(dropExtension, (</>), (<.>))
import System.Exit(exitSuccess, exitWith, ExitCode(..))

import CommonCLI

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
capitalize (s:ss) = toUpper s:ss

main :: IO ()
main  = do
  filenames <-  filter (isSuffixOf ".json")
            <$> getRecursiveContents "examples"
  results   <- forM filenames $ \filename -> do
    let outputFilename = capitalize (dropExtension filename) <.> "hs"
    genResult <- runghc ["GenerateJSONParser.hs", filename, "--outputFilename", outputFilename]
    return 0
    if genResult == ExitSuccess
      then return 0 -- number of failures so far
      else do
        parserResult <- runghc [outputFilename, filename]
        if parserResult == ExitSuccess
           then return 0
           else return 1
  exitCode $ sum results

exitCode  :: Int -> IO ()
exitCode 0 = exitSuccess
exitCode n = exitWith $ ExitFailure n

