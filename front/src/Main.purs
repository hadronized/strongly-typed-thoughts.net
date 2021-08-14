module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff $ do
  HA.awaitBody >>= runUI rootComponent unit

data Action = Increment | Decrement

rootComponent :: forall query input output. H.Component query input output Aff
rootComponent =
   H.mkComponent
    { -- First, we provide our function that describes how to produce the first state
      initialState
      -- Then, we provide our function that describes how to produce HTML from the state
    , render
      -- Finally, we provide our function that describes how to handle actions that
      -- occur while the component is running, which updates the state.
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
    initialState _ = 0
    render state = HH.div [] [
        HH.button [HE.onClick $ \_ -> Decrement] [HH.text "-1"],
        HH.text (show state),
        HH.button [HE.onClick $ \_ -> Increment] [HH.text "+1"]
      ]
    handleAction = case _ of
      Decrement -> H.modify_ (_-1)
      Increment -> H.modify_ (_+1)
