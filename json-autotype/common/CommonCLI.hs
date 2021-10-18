module CommonCLI(TypeOpts(..), unflag, tyOptParser) where


import           Data.Monoid                    ((<>))
import           Options.Applicative
import           System.Process                 (system)
import qualified System.Environment             (lookupEnv)
import           System.Exit                    (ExitCode)

import           Data.Aeson.AutoType.CodeGen    (Lang(..))

data TypeOpts = TyOptions {
                  autounify :: Bool
                , toplevel  :: String
                , debug     :: Bool
                , test      :: Bool
                , suggest   :: Bool
                , lang      :: Lang
                }

unflag :: Mod FlagFields Bool -> Parser Bool
unflag  = flag True False

tyOptParser :: Parser TypeOpts
tyOptParser  = TyOptions
            <$> unflag (long "no-autounify" <> help "Do not automatically unify suggested candidates")
            <*> strOption (short 't'        <>
                           long "toplevel"  <> value "TopLevel"
                                            <> help "Name for toplevel data type")
            <*> switch (long "debug"        <> help "Set this flag to see more debugging info"       )
            <*> unflag (long "no-test"      <> help "Do not run generated parser afterwards"         )
            <*> unflag (long "no-suggest"   <> help "Do not suggest candidates for unification"      )
            <*> langOpts


langOpts :: Parser Lang
langOpts  =  flag Haskell Haskell (long "haskell")
         <|> flag Haskell Elm     (long "elm")
         <|> flag Haskell Clang   (long "clang")

