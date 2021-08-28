-- | API endpoints and handlers.
module API
  ( API,
    Version,
    GPGAPI,
    MainBlogAPI,
    ComponentAPI,
    BlogAPI,
    BlogListingAPI,
    BlogArticleAPI,
    FeedAPI,
    RunAPI,
    runAPI,
    runServerAPI,
  )
where

import Blog (ArticleError (..), ArticleMetadata, LiftArticleError (..), Slug)
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Servant.API (Capture, Get, JSON, PlainText, Raw, (:<|>) (..), (:>))
import Servant.HTML.Blaze (HTML)
import Servant.Server (ServerError (..), err400, err404)
import Text.Blaze.Html (Html)
import Text.RSS.Syntax (RSS)
import XML (XML)

type Version = "v1"

type API =
  "api" :> Version :> MainAPI
    :<|> ComponentAPI
    :<|> StaticAPI
    :<|> RootAPI

type StaticAPI = "static" :> Raw

type RootAPI = Raw

type MainAPI =
  "media" :> Raw
    :<|> "pub" :> Raw
    :<|> "gpg" :> GPGAPI
    :<|> "blog" :> MainBlogAPI

-- This API is used to forward to /, but lists all the possible paths that can happen here and trigger components on the
-- front-end side.
type ComponentAPI =
  Get '[HTML] Html
    :<|> "blog" :> Get '[HTML] Html
    :<|> "blog" :> Capture "slug" Text :> Get '[HTML] Html
    :<|> "browse" :> Get '[HTML] Html

type GPGAPI = Get '[PlainText] Text

type MainBlogAPI = "feed" :> FeedAPI :<|> BlogAPI

type FeedAPI = Get '[XML] RSS

type BlogAPI =
  BlogListingAPI
    :<|> BlogArticleAPI

type BlogListingAPI = Get '[JSON] [ArticleMetadata]

type BlogArticleAPI = Capture "slug" Slug :> Get '[HTML] Html

-- | Main API error; i.e. all the possible errors that can occur.
newtype APIError
  = ArticleError ArticleError
  deriving (Eq, Show)

instance LiftArticleError APIError where
  liftArticleError = ArticleError

-- | Convert an 'APIError' into a 'Servant' error.
serverError :: APIError -> ServerError
serverError = \case
  ArticleError e -> case e of
    CannotDecodeMetadata reason -> err err400 $ "cannot decode blog article metadata: " <> reason
    CannotTranslate mkp reason -> err err400 $ "cannot translate " <> show mkp <> " blog article: " <> reason
    UnknownMarkup mkp -> err err404 $ "unknown blog article markup: " <> show mkp
    UnknownSlug slug -> err err404 $ "unknown blog article slug: " <> show slug
  where
    err e reason = e {errBody = fromString reason}

-- | Main API monad in which all the API code is going to run it.
newtype RunAPI a = RunAPI
  {unAPI :: ExceptT APIError IO a}
  deriving (Applicative, Functor, Monad, MonadIO, MonadError APIError)

runAPI :: RunAPI a -> IO (Either APIError a)
runAPI = runExceptT . unAPI

runServerAPI :: (MonadIO m, MonadError ServerError m) => RunAPI a -> m a
runServerAPI = liftIO . runAPI >=> either (throwError . serverError) pure
