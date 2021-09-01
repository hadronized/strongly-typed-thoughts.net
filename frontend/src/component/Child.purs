-- | Child component types and utility functions.
module Child (Query(..), refresh) where

import Prelude

-- | Query components can run on their children to request various useful information.
data Query a
  -- | Ask the child component to refresh.
  = Refresh a

-- | Refresh request.
refresh :: Query Unit
refresh = Refresh unit
