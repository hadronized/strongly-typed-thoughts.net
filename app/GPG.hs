{-# LANGUAGE DataKinds #-}

module GPG (
  GPGApi,
  serveGPGKeys
) where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import Data.Text.IO as T (readFile)
import Servant (Get, PlainText)
import Servant.Server (Server)

type GPGApi = Get '[PlainText] Text

serveGPGKeys :: FilePath -> Server (Get '[PlainText] Text)
serveGPGKeys = liftIO . T.readFile
