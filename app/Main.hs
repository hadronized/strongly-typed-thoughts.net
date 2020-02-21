import Control.Concurrent.STM.TVar (newTVarIO)
import Data.List (isSuffixOf)
import Data.Yaml (decodeFileEither)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)

import Blog (defaultBlogEntryMapping, refreshBlog)
import FileBrowser (defaultPubList, refreshBrowserFiles)
import ServerConfig (ServerConfig(..))
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.FSNotify (eventPath, watchTree, withManager)
import WebApp (webApp)

main :: IO ()
main = do
  eitherConf <- decodeFileEither "server.yaml"

  case eitherConf of
    Left e -> print e
    Right conf -> do
      let port = configPort conf
      let mediaDir = configMediaDir conf
      let uploadDir = configUploadDir conf
      let blogManifestPath = configBlogEntriesPath conf
      let blogDir = takeDirectory blogManifestPath
      let gpgKeyPath = configGPGKeyFile conf

      putStrLn $ "starting server on port " ++ show port

      -- create the directory to contain uploads if it doesn’t exist yet
      createDirectoryIfMissing True uploadDir

      -- create a TVar to hold browser’s files; those will get reloaded everytime a new file is pushed
      -- and at initialization
      filesTVar <- newTVarIO defaultPubList
      refreshBrowserFiles filesTVar

      -- create a TVar to hold the blog’s entries
      blogTVar <- newTVarIO defaultBlogEntryMapping
      refreshBlog blogManifestPath blogTVar

      -- watch for directories and hot-reload what must be
      putStrLn $ "listening to events in " ++ mediaDir
      withManager $ \mgr -> do
        _ <- watchTree mgr mediaDir (const True) $ \event -> do
          let parentDir = takeDirectory (eventPath event)

          if uploadDir `isSuffixOf` parentDir
          then
            refreshBrowserFiles filesTVar
          else if blogDir `isSuffixOf` parentDir
          then
            refreshBlog blogManifestPath blogTVar
          else
            pure ()

        let serverSettings = setLogger logger . setPort (fromIntegral port) $ defaultSettings
            logger req st _ = putStrLn $ show st ++ " | " ++ show req ++ "\n"
        runSettings serverSettings (webApp filesTVar uploadDir blogTVar gpgKeyPath)
