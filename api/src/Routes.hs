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

import API (API, BlogArticleAPI, BlogListingAPI, ComponentAPI, FeedAPI, GPGAPI, MainBlogAPI, runServerAPI)
import Config (Config (..))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getCurrentTime)
import Feed (rssFeed, rssItem)
import Servant (Server)
import Servant.API (Raw, (:<|>) (..))
import Servant.Server.StaticFiles (serveDirectoryFileServer, serveDirectoryWebApp)
import State (APIState, cachedIndexHtml, getBlogArticleContent, listBlogArticleMetadata)

routes :: Config -> APIState -> Server API
routes config state = mainAPI :<|> component state :<|> static :<|> root config
  where
    mainAPI =
      media config
        :<|> pub config
        :<|> gpgKeyFile config
        :<|> blog state

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

gpgKeyFile :: Config -> Server GPGAPI
gpgKeyFile = liftIO . T.readFile . configGPGKeyFile

blog :: APIState -> Server MainBlogAPI
blog state = feed state :<|> blogArticle
  where
    blogArticle = blogListing state :<|> article state

feed :: APIState -> Server FeedAPI
feed state = do
  now <- liftIO getCurrentTime
  items <- fmap (map rssItem) (listBlogArticleMetadata state)
  pure $ rssFeed now items

blogListing :: APIState -> Server BlogListingAPI
blogListing = listBlogArticleMetadata

article :: APIState -> Server BlogArticleAPI
article state slug = runServerAPI $ getBlogArticleContent state slug
