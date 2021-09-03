-- | JSON helpers.
module JSON (extractField) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.Maybe (Maybe, maybe)
import Foreign.Object (Object, lookup)

extractField :: forall a. Object Json -> String -> String -> (Json -> Maybe a) -> Either String a
extractField o errMsg field adapt = maybe (Left errMsg) pure (lookup field o >>= adapt)
