-- | API related functions.
module API (endpoint) where

import Prelude

endpoint :: String -> String
endpoint x = "/api" <> x
