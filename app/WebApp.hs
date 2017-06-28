{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Webapp (
    webapp
  ) where

import Control.Concurrent.STM.TVar (TVar)
import Servant (Proxy(..), Raw, (:>), (:<|>)(..))
import Servant.Server (Application, Server, serve)
import Servant.Utils.StaticFiles (serveDirectoryWebApp)

import FileBrowser (FileBrowserApi, PubList, fileBrowser)
import Home (HomeApi, home)
import Portfolio (PortfolioApi, portfolio)

webapp :: TVar PubList -> Application
webapp = serve (Proxy :: Proxy Api) . server

type API =
       HomeApi
  :<|> "portfolio" :> PortfolioApi
  :<|> "browse" :> FileBrowserApi
  :<|> "media" :> Raw
  :<|> "static" :> Raw

server :: TVar PubList -> Server Api
server filesTVar =
       home
  :<|> portfolio
  :<|> fileBrowser filesTVar
  :<|> serveDirectoryWebApp "media"
  :<|> serveDirectoryWebApp "static"
