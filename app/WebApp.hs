{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebApp (
    webApp
  ) where

import Control.Concurrent.STM.TVar (TVar)
import Servant (Proxy(..), Raw, (:>), (:<|>)(..))
import Servant.Server (Application, Server, serve)
import Servant.Utils.StaticFiles (serveDirectoryWebApp)

import Blog (BlogApi, blog)
import FileBrowser (FileBrowserApi, PubList, fileBrowserHandler)
import Home (HomeApi, home)
import Portfolio (PortfolioApi, portfolio)

webApp :: TVar PubList -> FilePath -> Application
webApp filesTVar uploadDir = serve (Proxy :: Proxy Api) (server filesTVar uploadDir)

type Api =
       HomeApi
  :<|> "portfolio" :> PortfolioApi
  :<|> "browse" :> FileBrowserApi
  :<|> "media" :> Raw
  :<|> "static" :> Raw
  :<|> "pub" :> Raw -- legacy links
  :<|> "blog" :> BlogApi

server :: TVar PubList -> FilePath -> Server Api
server filesTVar uploadDir =
       home
  :<|> portfolio
  :<|> fileBrowserHandler filesTVar
  :<|> serveDirectoryWebApp "media"
  :<|> serveDirectoryWebApp "static"
  :<|> serveDirectoryWebApp uploadDir
  :<|> blog
