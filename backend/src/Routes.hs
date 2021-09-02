{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- | API server routes.
module Routes
  ( routes,
    root,
    media,
    pub,
    static,
    gpgKeyFile,
    blog,
  )
where

import API (API, BlogAPI, BlogArticleAPI, BlogListingAPI, ComponentAPI, FeedAPI, GPGAPI, runServerAPI)
import Config (Config (..))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text as T
import Data.Time.Clock.POSIX (getCurrentTime)
import Feed (rssFeed, rssItem)
import Servant (Server, err404)
import Servant.API (Raw, (:<|>) (..))
import Servant.Server.StaticFiles (serveDirectoryFileServer, serveDirectoryWebApp)
import State (APIState (cachedGPGKeyFile), cachedIndexHtml, getBlogArticleContent, listBlogArticleMetadata)

routes :: Config -> APIState -> Server API
routes config state =
  feed state
    :<|> media config
    :<|> pub config
    :<|> gpgKeyFile state
    :<|> component state
    :<|> static
    :<|> blog state
    :<|> root config

component :: APIState -> Server ComponentAPI
component state = serveRoot :<|> serveBlogListing :<|> serveBlogArticle :<|> serveBrowse
  where
    serveIndex = pure (cachedIndexHtml state)
    serveRoot = serveIndex
    serveBlogListing = serveIndex
    serveBlogArticle _ = serveIndex
    serveBrowse = serveIndex

root :: Config -> Server Raw
root = serveDirectoryWebApp . configFrontDir

media :: Config -> Server Raw
media = serveDirectoryFileServer . configMediaDir

pub :: Config -> Server Raw
pub = serveDirectoryFileServer . configUploadDir

static :: Server Raw
static = serveDirectoryWebApp "static"

gpgKeyFile :: APIState -> Server GPGAPI
gpgKeyFile state = if T.null content then throwError err404 else pure content
  where
    content = cachedGPGKeyFile state

blog :: APIState -> Server BlogAPI
blog state = blogListing state :<|> article state

feed :: APIState -> Server FeedAPI
feed state = do
  now <- liftIO getCurrentTime
  items <- fmap (map rssItem) (listBlogArticleMetadata state)
  pure $ rssFeed now items

blogListing :: APIState -> Server BlogListingAPI
blogListing = listBlogArticleMetadata

article :: APIState -> Server BlogArticleAPI
article state slug = runServerAPI $ getBlogArticleContent state slug
