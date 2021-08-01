module Main where

import Blog (ArticleError, LiftArticleError (..))
import Config (Config (..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS
import Data.List (isSuffixOf)
import State (newAPIState, statefulCacheFile, statefulUnCacheFile)
import System.Directory (createDirectoryIfMissing)
import System.FSNotify (Event (Added, Removed), eventPath, watchTree, withManager)
import System.FilePath (takeDirectory)

-- | Main API error; i.e. all the possible errors that can occur.
newtype APIError
  = ArticleError ArticleError
  deriving (Eq, Show)

instance LiftArticleError APIError where
  liftArticleError = ArticleError

-- | Main API monad in which all the API code is going to run it.
newtype API a = API
  {unAPI :: ExceptT APIError IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadError APIError)

runAPI :: API a -> IO (Either APIError a)
runAPI = runExceptT . unAPI

api :: Config -> API ()
api config = do
  -- let port = configPort config
  let mediaDir = configMediaDir config
  let uploadDir = configUploadDir config
  -- let blogDir = takeDirectory blogManifestPath
  -- let gpgKeyPath = configGPGKeyFile config

  -- create the directory to contain uploads if it doesn’t exist yet
  liftIO (createDirectoryIfMissing True uploadDir)

  -- create the API state
  apiState <- newAPIState config

  -- start a notify thread to listen for file changes
  liftIO . withManager $ \mgr -> do
    putStrLn $ "watching " <> mediaDir
    _ <- watchTree mgr mediaDir (const True) $ \event -> do
      putStrLn $ "event: " <> show event
      let path = eventPath event
      let parentDir = takeDirectory path

      if uploadDir `isSuffixOf` parentDir
        then case event of
          Added {} -> statefulCacheFile path apiState
          Removed {} -> statefulUnCacheFile path apiState
          _ -> pure ()
        else pure ()

    forever $ do
      threadDelay 1000000

main :: IO ()
main = do
  eitherConfig <- fmap eitherDecode (BS.readFile "server.json")
  case eitherConfig of
    Left err -> do
      putStrLn "cannot read configuration"
      print err
    Right config -> do
      runAPI (api config) >>= \case
        Left err -> do
          putStrLn $ "API failed with: " <> show err
        Right _ -> putStrLn "bye" -- if blogDir `isSuffixOf` parentDir
        --   then refreshBlog blogManifestPath blogTVar
        --   else pure ()SuffixOf` parentDir
        --   then refreshBlog blogManifestPath blogTVar
        --   else pure ()

-- let serverSettings = setLogger logger . setPort (fromIntegral port) $ defaultSettings
--     logger req st _ = putStrLn $ show st ++ " | " ++ show req ++ "\n"
-- runSettings serverSettings (webApp filesTVar uploadDir blogTVar gpgKeyPath)
