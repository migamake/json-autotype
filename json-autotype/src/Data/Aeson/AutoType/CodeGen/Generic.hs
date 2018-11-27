{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}
module Data.Aeson.AutoType.CodeGen.Generic(src) where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- | Multiline source string
src = QuasiQuoter (\src -> [|src|])
                  (error "Cannot use src as pattern")
                  (error "Cannot use src as type"   )
                  (error "Cannot use src as dec"    )

