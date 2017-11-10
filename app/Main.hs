import Network.Wai.Handler.Warp (run)

import Control.Concurrent.STM.TVar (newTVarIO)
import Data.Default (Default(..))
import Data.Maybe  (fromMaybe)
import Data.Yaml (decodeFile)
import FileBrowser (defaultPubList, refreshBrowserFiles)
import ServerConfig (ServerConfig(..))
import Webapp (webapp)

main :: IO ()
main = do
  ServerConfig port <- fmap (fromMaybe def) (decodeFile "server.yaml")
  putStrLn $ "starting server on port " ++ show port

  -- create a TVar to hold browser’s files; those will get reloaded everytime a new file is pushed
  -- and at initialization
  filesTVar <- newTVarIO defaultPubList
  refreshBrowserFiles filesTVar

  run (fromIntegral port) (webapp filesTVar)
