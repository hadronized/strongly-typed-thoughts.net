module Main where

import Prelude
import Data.Int (round)
import Data.JSDate (getFullYear, now)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import SPA (spaComponent)
import Router (defaultRouter)

main :: Effect Unit
main = do
  year <- map round (now >>= getFullYear)
  router <- defaultRouter
  HA.runHalogenAff $ HA.awaitBody >>= runUI (spaComponent year) router
