-- | Routing primitives.
-- |
-- | This module allows to query and set the routing path (i.e. basically the URL).
module Router
  ( Router
  , defaultRouter
  , path
  , setPath
  ) where

import Prelude
import Effect.Class (class MonadEffect, liftEffect)
import Foreign (Foreign, unsafeToForeign)
import Web.HTML (History, window)
import Web.HTML.History (DocumentTitle(..), URL(..), pushState)
import Web.HTML.Location (Location, pathname)
import Web.HTML.Window (history, location)

data Router
  = Router
    { location :: Location
    , history :: History
    , emptyForeign :: Foreign
    }

defaultRouter :: forall m. MonadEffect m => m Router
defaultRouter =
  liftEffect
    $ do
        win <- window
        router <- { location: _, history: _, emptyForeign: _ } <$> location win <*> history win <*> pure (unsafeToForeign unit)
        pure (Router router)

path :: forall m. MonadEffect m => Router -> m String
path (Router router) = liftEffect (pathname router.location)

setPath :: forall m. MonadEffect m => Router -> String -> m Unit
setPath (Router router) p = liftEffect (pushState router.emptyForeign (DocumentTitle p) (URL p) router.history)
