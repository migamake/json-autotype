module CommonCLI(TypeOpts(..), unflag, tyOptParser) where

import Data.Monoid((<>))
import Options.Applicative

data TypeOpts = TyOptions {
                  autounify :: Bool
                , debug     :: Bool
                , test      :: Bool
                , suggest   :: Bool
                }

unflag :: Mod FlagFields Bool -> Parser Bool
unflag  = flag True False

tyOptParser :: Parser TypeOpts
tyOptParser  = TyOptions
            <$> unflag (long "no-autounify" <> help "Do not automatically unify suggested candidates")
            <*> switch (long "debug"        <> help "Set this flag to see more debugging info"       )
            <*> unflag (long "no-test"      <> help "Do not run generated parser afterwards"         )
            <*> unflag (long "no-suggest"   <> help "Do not suggest candidates for unification"      )

