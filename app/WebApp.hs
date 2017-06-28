{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Webapp (
    webapp
  ) where

import Control.Concurrent.STM.TVar (TVar)
import Servant (Proxy(..), (:>), (:<|>)(..))
import Servant.Server (Application, Server, serve)

import FileBrowser (FileBrowserApi, PubList, fileBrowser)
import Home (HomeApi, home)
import Portfolio (PortfolioApi, portfolio)

webapp :: TVar PubList -> Application
webapp = serve (Proxy :: Proxy Api) . server

type Api =
       HomeApi
  :<|> "portfolio" :> PortfolioApi
  :<|> "browse" :> FileBrowserApi

server :: TVar PubList -> Server Api
server filesTVar = home :<|> portfolio :<|> fileBrowser filesTVar
