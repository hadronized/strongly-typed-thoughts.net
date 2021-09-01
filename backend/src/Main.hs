module Main where

import API (API, RunAPI, runAPI)
import Config (Config (..))
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS
import Data.Data (Proxy (..))
import Data.List (isSuffixOf)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Routes (routes)
import Servant.Server (serve)
import State (newAPIState, statefulCacheFile, statefulUnCacheFile)
import System.Directory (createDirectoryIfMissing)
import System.FSNotify (Event (..), eventPath, watchTree, withManager)
import System.FilePath (takeDirectory)

api :: Config -> RunAPI ()
api config = do
  let mediaDir = configMediaDir config
  let uploadDir = configUploadDir config

  -- create the directory to contain uploads if it doesn’t exist yet
  liftIO (createDirectoryIfMissing True uploadDir)

  -- create the API state
  apiState <- newAPIState config

  -- start a notify thread to listen for file changes
  liftIO . withManager $ \mgr -> do
    putStrLn $ "watching " <> mediaDir
    void . watchTree mgr mediaDir (const True) $ \event -> do
      putStrLn $ "event: " <> show event
      let path = eventPath event
      let parentDir = takeDirectory path

      if uploadDir `isSuffixOf` parentDir
        then case event of
          Added {} -> statefulCacheFile path apiState
          Removed {} -> statefulUnCacheFile path apiState
          _ -> pure ()
        else pure ()

    let serverSettings = setLogger logger . setPort (fromIntegral $ configPort config) $ defaultSettings
        logger req st _ = putStrLn $ show st ++ " | " ++ show req ++ "\n"
    liftIO . runSettings serverSettings $
      serve (Proxy :: Proxy API) (routes config apiState)

main :: IO ()
main = do
  eitherConfig <- fmap eitherDecode (BS.readFile "backend.json")
  case eitherConfig of
    Left err -> do
      putStrLn "cannot read configuration"
      print err
    Right config -> do
      runAPI (api config) >>= \case
        Left err -> do
          putStrLn $ "API failed with: " <> show err
        Right _ -> putStrLn "bye"
