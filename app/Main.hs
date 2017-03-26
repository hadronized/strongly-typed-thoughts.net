import Network.Wai.Handler.Warp (run)

import Data.Default (Default(..))
import Data.Maybe  (fromMaybe)
import Data.Yaml (decodeFile)
import ServerConfig (ServerConfig(..))
import Webapp (webapp)

main :: IO ()
main = do
  ServerConfig port <- fmap (fromMaybe def) (decodeFile "server.yaml")
  putStrLn $ "starting server on port " ++ show port

  run (fromIntegral port) webapp
