-- | API related functions.
module API (endpoint) where

import Prelude

endpoint :: String -> String
endpoint x = "/api/v1" <> x
