-- | Blog listing, searching, reading. All the things.
module Blog where

import Prelude
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML (text)

blogComponent :: forall query input output m. Component query input output m
blogComponent = mkComponent { eval, initialState, render }
  where
  eval = mkEval defaultEval

  initialState _ = unit

  render _ = text "lol"
