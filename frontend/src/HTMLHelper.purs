-- | A small module to provide functions to build HTML nodes and their properties in a faster way.
module HTMLHelper where

import Prelude

import Halogen (ClassName(..))
import Halogen.HTML.Properties (IProp, classes)

cl :: forall r i. Array String -> IProp (class :: String | r) i
cl = classes <<< map ClassName
