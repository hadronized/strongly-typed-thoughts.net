import Network.Wai.Handler.Warp (run)

import Control.Concurrent.STM.TVar (newTVarIO)
import Data.Default (Default(..))
import Data.Maybe  (fromMaybe)
import Data.Yaml (decodeFile)

import Blog (defaultBlogEntryMapping, refreshBlog)
import FileBrowser (defaultPubList, refreshBrowserFiles)
import ServerConfig (ServerConfig(..))
import System.Directory (createDirectoryIfMissing)
import WebApp (webApp)

main :: IO ()
main = do
  conf <- fmap (fromMaybe def) (decodeFile "server.yaml")
  let port = configPort conf
  let uploadDir = configUploadDir conf
  let blogManifestPath = configBlogEntriesPath conf

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

  run (fromIntegral port) (webApp filesTVar uploadDir blogManifestPath blogTVar)
