{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Config (Config (..)) where

import Data.Default (Default (..))
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data Config = Config
  { configPort :: Natural,
    configMediaDir :: FilePath,
    configUploadDir :: FilePath,
    configBlogEntriesPath :: FilePath,
    configGPGKeyFile :: FilePath
  }
  deriving (Eq, Generic, Show)

instance FromJSON Config

instance ToJSON Config

instance Default Config where
  def =
    Config
      { configPort = 8000,
        configMediaDir = "media",
        configUploadDir = "media/uploads",
        configBlogEntriesPath = "media/blog/entries.yaml",
        configGPGKeyFile = "media/gpg/phaazon.gpg"
      }
