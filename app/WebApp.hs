{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module WebApp (
    webApp
  ) where

import Control.Monad.Error.Class (MonadError(..))
import Control.Concurrent.STM.TVar (TVar)
import Servant (Get, NoContent(..), Proxy(..), Raw, (:>), (:<|>)(..))
import Servant.HTML.Blaze (HTML)
import Servant.Server (Application, Server, ServerError(..), err301, serve)
import Servant.Server.StaticFiles (serveDirectoryWebApp)

import Blog (BlogApi, BlogEntryMapping, blog)
import Feed (FeedApi, feed)
import FileBrowser (FileBrowserApi, PubList, fileBrowserHandler)
import GPG (GPGApi, serveGPGKeys)
import Home (HomeApi, home)

webApp :: TVar PubList -> FilePath -> FilePath -> TVar BlogEntryMapping -> FilePath -> Application
webApp filesTVar uploadDir blogManifestPath blogEntryMapping gpgKeyPath =
  serve (Proxy :: Proxy Api) (server filesTVar uploadDir blogManifestPath blogEntryMapping gpgKeyPath )

type Api =
       HomeApi
  :<|> "browse" :> FileBrowserApi
  :<|> "media" :> Raw
  :<|> "static" :> Raw
  :<|> "pub" :> Raw -- legacy links
  :<|> "blog" :> ("feed" :> FeedApi :<|> BlogApi)
  :<|> "gpg" :> GPGApi
  :<|> "lost" :> LostApi

server :: TVar PubList -> FilePath -> FilePath -> TVar BlogEntryMapping -> FilePath -> Server Api
server filesTVar uploadDir blogManifestPath blogEntryMapping gpgKeyPath =
       home
  :<|> fileBrowserHandler filesTVar
  :<|> serveDirectoryWebApp "media"
  :<|> serveDirectoryWebApp "static"
  :<|> serveDirectoryWebApp uploadDir
  :<|> (feed blogEntryMapping :<|> blog blogManifestPath blogEntryMapping)
  :<|> serveGPGKeys gpgKeyPath
  :<|> serveLost

-- it’s just all fun :D
type LostApi = Get '[HTML] NoContent

serveLost :: Server LostApi
serveLost = throwError $ err301 { errHeaders = [("Location", "https://phaazon.net/media/uploads/dylan.gif")] }
