{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebApp (
    webApp
  ) where

import Control.Concurrent.STM.TVar (TVar)
import Servant (Proxy(..), Raw, (:>), (:<|>)(..))
import Servant.Server (Application, Server, serve)
import Servant.Server.StaticFiles (serveDirectoryWebApp)

import Blog (BlogApi, BlogEntryMapping, blog)
import Feed (FeedApi, feed)
import FileBrowser (FileBrowserApi, PubList, fileBrowserHandler)
import GPG (GPGApi, serveGPGKeys)
import Home (HomeApi, home)
import Portfolio (PortfolioApi, portfolio)

webApp :: TVar PubList -> FilePath -> FilePath -> TVar BlogEntryMapping -> FilePath -> Application
webApp filesTVar uploadDir blogManifestPath blogEntryMapping gpgKeyPath =
  serve (Proxy :: Proxy Api) (server filesTVar uploadDir blogManifestPath blogEntryMapping gpgKeyPath )

type Api =
       HomeApi
  :<|> "portfolio" :> PortfolioApi
  :<|> "browse" :> FileBrowserApi
  :<|> "media" :> Raw
  :<|> "static" :> Raw
  :<|> "pub" :> Raw -- legacy links
  :<|> "blog" :> ("feed" :> FeedApi :<|> BlogApi)
  :<|> "gpg" :> GPGApi

server :: TVar PubList -> FilePath -> FilePath -> TVar BlogEntryMapping -> FilePath -> Server Api
server filesTVar uploadDir blogManifestPath blogEntryMapping gpgKeyPath =
       home
  :<|> portfolio
  :<|> fileBrowserHandler filesTVar
  :<|> serveDirectoryWebApp "media"
  :<|> serveDirectoryWebApp "static"
  :<|> serveDirectoryWebApp uploadDir
  :<|> (feed blogEntryMapping :<|> blog blogManifestPath blogEntryMapping)
  :<|> serveGPGKeys gpgKeyPath
