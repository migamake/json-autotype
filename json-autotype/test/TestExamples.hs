-- Test over all files in examples/ directory
module Main(main) where

import Control.Monad(forM, forM_, unless, join)
import Data.Char(toUpper)
import Data.Functor ((<$>))
import Data.List(isPrefixOf, isSuffixOf)
import System.Directory(doesDirectoryExist, getDirectoryContents, createDirectoryIfMissing)
import System.FilePath((</>), (<.>), takeBaseName, replaceFileName)
import System.Exit(ExitCode(..))
import System.Environment as Env
import System.Process             (rawSystem)
import Data.Aeson.AutoType.CodeGen(runModule, Lang(Haskell))
import Data.Aeson ( Result,  Object, FromJSON, Value(Null,Number), (.:?) )
import Data.Aeson.Types ( Parser, parse )
import Data.Text ( Text, pack )
import Data.HashMap.Lazy ( singleton, empty )


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
  putStrLn "****************************************"
  verifyAesonOperators
  filenames <-  filter (isSuffixOf ".json")
            <$> getRecursiveContents "examples"
  createDirectoryIfMissing True "output"
  forM_ filenames $ \filename -> do
    let outputFilename = ("output" </> capitalize (takeBaseName filename <.> "hs"))
    genResult <- runAutotype [filename, "--outputFilename", outputFilename]
    unless (genResult == ExitSuccess) $
      fail (unwords ["test case", show filename, "failed with", show genResult])
    parserResult <- runModule Haskell outputFilename [filename]
    --            ^ runModule HaskellStrict -- for compiling with -Wall -Werror
    unless (parserResult == ExitSuccess) $
      fail (unwords ["generated parser", show outputFilename, "failed with", show parserResult])

runAutotype :: [String] -> IO ExitCode
runAutotype arguments = do
    stackEnv   <- doesDirectoryExist ".stack-work"
    cabalEnv   <- doesDirectoryExist "dist/build/autogen"
    maybeStack <- Env.lookupEnv "STACK_EXEC"
    let (exec, args) | Just stackExec <- maybeStack = (stackExec, ["run","--"             ])
                     | stackEnv                     = ("stack",   ["run","--"             ])
                     | cabalEnv                     = ("cabal",   ["run","--"             ])
                     | otherwise                    = error "This test must be run either in Stack or Cabal environment."
    putStrLn $ concat ["Running json-autotype with executable ", show exec, " and arguments ", show args]
    rawSystem exec $ args ++ arguments

verifyAesonOperators :: IO ()
verifyAesonOperators = do
  parseTest (singleton (pack "foo") (Number 1))
  parseTest (singleton (pack "foo")  Null     )
  parseTest (singleton (pack "bar")  Null     )
  parseTest empty

(.:??) :: FromJSON a => Object -> Text -> Parser (Maybe a)
o .:?? val = fmap join (o .:? val)

parseTest :: Object -> IO ()
parseTest o = unless (r1 == r2) (fail (show r1 ++ " /= " ++ show r2))
  where r1, r2 :: Result (Maybe Int)
        r1 = parse (.:? (pack "foo")) o
        r2 = parse (.:?? (pack "foo")) o
