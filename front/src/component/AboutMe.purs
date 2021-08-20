-- | About me component, displaying information about who I am.
module AboutMe where

import Prelude

import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML (text)

aboutMeComponent :: forall query input output m. Component query input output m
aboutMeComponent = mkComponent { eval, initialState, render }
  where
    eval = mkEval defaultEval
    initialState _ = unit
    render _ = text "It’s a me, DiMitri"
