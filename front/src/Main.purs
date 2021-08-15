module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen (ClassName(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Tile (tileComponent)
import Type.Proxy (Proxy(..))

main :: Effect Unit
main = HA.runHalogenAff $ HA.awaitBody >>= runUI rootComponent unit

type Slots
  = ( tile :: forall query. H.Slot query Void Int )

_tile = Proxy :: Proxy "nested"

rootComponent :: forall query input output. H.Component query input output Aff
rootComponent =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState _ = 0

  render _ =
    HH.div [ HP.classes $ map ClassName [ "tile-container" ] ]
      [ HH.slot_ _tile 0 tileComponent "upper-left"
      , HH.slot_ _tile 1 tileComponent "upper-right"
      , HH.slot_ _tile 2 tileComponent "lower-left"
      , HH.slot_ _tile 3 tileComponent "lower-right"
      ]
