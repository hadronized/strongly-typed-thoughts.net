module Main where

import Prelude
import Data.Int (round)
import Data.JSDate (getFullYear, now)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import SPA (spaComponent)

main :: Effect Unit
main = do
  year <- map round (now >>= getFullYear)
  HA.runHalogenAff $ HA.awaitBody >>= runUI (spaComponent year) unit
