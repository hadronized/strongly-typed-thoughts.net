module Main where

import Config (Config (..))
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.List (isSuffixOf)
import Data.Yaml (decodeFileEither)
import State (newAPIState, statefulCacheFile, statefulUnCacheFile)
import System.Directory (createDirectoryIfMissing)
import System.FSNotify (Event (Added, Removed), eventPath, watchTree, withManager)
import System.FilePath (takeDirectory)

main :: IO ()
main =
  decodeFileEither "server.yaml" >>= \case
    Left err -> do
      putStrLn "cannot read configuration"
      print err
    Right (conf :: Config) -> do
      print conf
      let port = configPort conf
      let mediaDir = configMediaDir conf
      let uploadDir = configUploadDir conf
      let blogManifestPath = configBlogEntriesPath conf
      let blogDir = takeDirectory blogManifestPath
      let gpgKeyPath = configGPGKeyFile conf

      -- create the directory to contain uploads if it doesn’t exist yet
      createDirectoryIfMissing True uploadDir

      -- create the API state
      apiState <- newAPIState

      -- start a notify thread to listen for file changes
      withManager $ \mgr -> do
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

-- if blogDir `isSuffixOf` parentDir
--   then refreshBlog blogManifestPath blogTVar
--   else pure ()

-- let serverSettings = setLogger logger . setPort (fromIntegral port) $ defaultSettings
--     logger req st _ = putStrLn $ show st ++ " | " ++ show req ++ "\n"
-- runSettings serverSettings (webApp filesTVar uploadDir blogTVar gpgKeyPath)
