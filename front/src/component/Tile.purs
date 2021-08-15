-- A tile component, which contains HTML.
module Tile where

import Prelude (($), map)
import Halogen (ClassName(..), Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML (div, text)
import Halogen.HTML.Properties as HP

tileComponent :: forall query output m. Component query String output m
tileComponent = mkComponent { eval, initialState, render }
  where
  eval = mkEval defaultEval

  initialState = text

  render state =
    div [ HP.classes $ map ClassName [ "tile-component" ] ]
      [ state ]
