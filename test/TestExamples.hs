-- Test over all files in examples/ directory
module Main(main) where

import System.FilePath(dropExtension)
import System.Exit(ExitCode(..))

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

main :: IO ()
main  = do
  filenames <- getRecursiveContents "examples"
  results <- forM filenames $ \filename -> do
    -- basename $i
    -- echo stack exec -- ${EXE} $i
    -- OUT=`basename $i .json`.hs
    -- time stack exec -- ${EXE} $i --outputFilename ${OUT} && stack exec -- ghc ${GHCOPTS} ${OUT} || exit 1
    -- stack exec -- runghc -- ${GHCOPTS} ${OUT} ${i} || exit 2
    let outputFilename = dropExtension filename <.> "hs"
    genResult <- runghc ["GenerateJSONParser.hs", filename, "--outputFilename", outputFilename]
    if genResult == ExitSuccess
      then return 0 -- number of failures so far
      else do
        parserResult <- runghc ["GenerateJSONParser.hs", outputFilename, "--outputFilename", outputFilename]
        return $ if parserResult == ExitSuccess
                    then 0
                    else 1
    exitFailure $ sum results

