{-# LANGUAGE DeriveGeneric #-}

module ServerConfig (
  ServerConfig(..)
) where

import Data.Default (Default(..))
import Data.Yaml (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

data ServerConfig = ServerConfig {
    configPort :: Natural
  , configUploadDir :: FilePath
  } deriving (Eq, Generic, Show)

instance FromJSON ServerConfig
instance ToJSON ServerConfig

instance Default ServerConfig where
  def = ServerConfig {
      configPort = 8000
    , configUploadDir = "media/uploads"
    }
