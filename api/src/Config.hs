{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Config (Config (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding, genericToJSON)
import Data.Default (Default (..))
import GHC.Generics (Generic)
import JSON (jsonOptions)
import Numeric.Natural (Natural)

data Config = Config
  { configPort :: Natural,
    configMediaDir :: FilePath,
    configUploadDir :: FilePath,
    configBlogIndex :: FilePath,
    configGPGKeyFile :: FilePath
  }
  deriving (Eq, Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON Config where
  toEncoding = genericToEncoding jsonOptions
  toJSON = genericToJSON jsonOptions

instance Default Config where
  def =
    Config
      { configPort = 8000,
        configMediaDir = "media",
        configUploadDir = "media/uploads",
        configBlogIndex = "media/blog/index.json",
        configGPGKeyFile = "media/gpg/phaazon.gpg"
      }
