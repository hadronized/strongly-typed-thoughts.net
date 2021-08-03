module Main where

import Blog (ArticleError, LiftArticleError (..))
import Config (Config (..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS
import Data.Data (Proxy (..))
import Data.List (isSuffixOf)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import State (newAPIState, statefulCacheArticle, statefulCacheFile, statefulUnCacheFile)
import System.Directory (createDirectoryIfMissing)
import System.FSNotify (Event (..), eventPath, watchTree, withManager)
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

ioRunAPI :: API a -> IO ()
ioRunAPI a =
  runAPI a >>= \case
    Left e -> putStrLn $ "API error: " <> show e
    Right _ -> pure ()

api :: Config -> API ()
api config = do
  let mediaDir = configMediaDir config
  let uploadDir = configUploadDir config
  let articleIndexPath = configBlogIndex config
  let blogDir = takeDirectory articleIndexPath
  -- let gpgKeyPath = configGPGKeyFile config

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
        else
          if blogDir `isSuffixOf` parentDir
            then case event of
              Added {} -> ioRunAPI (statefulCacheArticle path apiState)
              -- Modified {} -> ioRunAPI (statefulChangeArticle path apiState)
              _ -> pure ()
            else pure ()

-- let serverSettings = setLogger logger . setPort (fromIntegral $ configPort config) $ defaultSettings
--     logger req st _ = putStrLn $ show st ++ " | " ++ show req ++ "\n"
--     gpgKeyPath = configGPGKeyFile config
-- runSettings serverSettings $
--   serve (Proxy :: Proxy API) (routes filesTVar uploadDir blogEntryMapping gpgKeyPath)

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
        Right _ -> putStrLn "bye"

-- let serverSettings = setLogger logger . setPort (fromIntegral port) $ defaultSettings
--     logger req st _ = putStrLn $ show st ++ " | " ++ show req ++ "\n"
-- runSettings serverSettings (webApp filesTVar uploadDir blogTVar gpgKeyPath)
