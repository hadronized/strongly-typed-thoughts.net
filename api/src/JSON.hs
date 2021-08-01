-- | Common JSON settings.
module JSON
  ( jsonOptions,
  )
where

import Data.Aeson (Options (..), camelTo2, defaultOptions)

jsonOptions :: Options
jsonOptions =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_'
    }
