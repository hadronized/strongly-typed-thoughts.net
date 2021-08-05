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

import API (API, BlogArticleAPI, BlogListingAPI, GPGAPI, MainBlogAPI, runServerAPI)
import Config (Config (..))
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Text.IO as T
import Servant (Server)
import Servant.API (Raw, (:<|>) (..))
import Servant.Server.StaticFiles (serveDirectoryFileServer, serveDirectoryWebApp)
import State (APIState, getBlogArticleContent, listBlogArticleMetadata)

routes :: Config -> APIState -> Server API
routes config state =
  media config
    :<|> pub config
    :<|> static
    :<|> gpgKeyFile config
    :<|> blog state
    :<|> root

root :: Server Raw
root = serveDirectoryWebApp "front"

media :: Config -> Server Raw
media = serveDirectoryFileServer . configMediaDir

pub :: Config -> Server Raw
pub = serveDirectoryFileServer . configUploadDir

static :: Server Raw
static = serveDirectoryFileServer "static"

gpgKeyFile :: Config -> Server GPGAPI
gpgKeyFile = liftIO . T.readFile . configGPGKeyFile

blog :: APIState -> Server MainBlogAPI
blog state = {- feed :<|> -} blogArticle
  where
    blogArticle = blogListing state :<|> article state

blogListing :: APIState -> Server BlogListingAPI
blogListing = listBlogArticleMetadata

article :: APIState -> Server BlogArticleAPI
article state slug = runServerAPI $ getBlogArticleContent state slug