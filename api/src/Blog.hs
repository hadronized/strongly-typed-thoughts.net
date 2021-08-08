-- Blog articles.
module Blog
  ( ArticleMetadata (..),
    articleLastModificationDate,
    ArticleMetadataStore,
    Slug (..),
    storeFromMetadata,
    metadataArticles,
    getCacheArticle,
    readMetadata,
    updateModificationDate,
    getArticleContent,
    LiftArticleError (..),
    ArticleError (..),
  )
where

import Control.Monad (foldM)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON (..), FromJSONKey, ToJSON (..), ToJSONKey, eitherDecode, genericParseJSON, genericToEncoding, genericToJSON)
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import JSON (jsonOptions)
import Markup (Markup (..), markupToHtml)
import Servant (FromHttpApiData (..))
import System.Directory.Internal.Prelude (fromMaybe)
import System.FilePath (takeExtension)
import Text.Blaze.Html (Html)

-- | Type of error that can occur while dealing with blog articles.
data ArticleError
  = CannotDecodeMetadata String
  | CannotTranslate Markup String
  | UnknownMarkup String
  | UnknownSlug Slug
  deriving (Eq, Show)

class LiftArticleError e where
  liftArticleError :: ArticleError -> e

-- Article slug (i.e. short unique name to represent it in stores, URL links, etc.).
newtype Slug = Slug {unSlug :: Text} deriving (Eq, FromJSON, FromJSONKey, Generic, Hashable, Show, ToJSON, ToJSONKey)

instance FromHttpApiData Slug where
  parseUrlPiece = Right . Slug

-- Metadata about a blog article.
--
-- Blog metadata are used as a cache of entries in a metadata store to be able to know more about a given blog article.
data ArticleMetadata = ArticleMetadata
  { -- Name of the entry. It’s used in listings, for instance.
    articleName :: Text,
    -- Path of the article.
    articlePath :: FilePath,
    -- Date when the article was published.
    articlePublishDate :: UTCTime,
    -- Last date modification of the article.
    articleModificationDate :: Maybe UTCTime,
    -- Some tags.
    articleTags :: [Text],
    -- Slug used in URL to refer to that article.
    articleSlug :: Slug
  }
  deriving (Eq, Generic, Show)

instance FromJSON ArticleMetadata where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ArticleMetadata where
  toEncoding = genericToEncoding jsonOptions
  toJSON = genericToJSON jsonOptions

-- | Get the last time the article was modified.
articleLastModificationDate :: ArticleMetadata -> UTCTime
articleLastModificationDate article = fromMaybe (articlePublishDate article) (articleModificationDate article)

-- A cached article is the combination of metadata about the article as well as the content of the article optionally
-- loaded.
data CachedArticle = CachedArticle
  { cachedArticleMetadata :: ArticleMetadata,
    cachedArticleContent :: Html
  }
  deriving (Generic)

-- Article metadata store.
--
-- Store article in a map-like data type, keyed by slug.
newtype ArticleMetadataStore
  = ArticleMetadataStore (H.HashMap Slug CachedArticle)
  deriving (Generic)

readMetadata :: (MonadIO m, MonadError e m, LiftArticleError e) => FilePath -> m [ArticleMetadata]
readMetadata path = do
  content <- liftIO (BS.readFile path)
  either (throwError . liftArticleError . CannotDecodeMetadata) pure $ eitherDecode content

storeFromMetadata :: (MonadIO m, MonadError e m, LiftArticleError e) => [ArticleMetadata] -> m ArticleMetadataStore
storeFromMetadata =
  foldM (\h metadata -> snd <$> getCacheArticle h metadata) (ArticleMetadataStore H.empty)

-- Get a listing of articles metadata.
metadataArticles :: ArticleMetadataStore -> [ArticleMetadata]
metadataArticles (ArticleMetadataStore h) = map cachedArticleMetadata (H.elems h)

-- Update last modification date of an article.
updateModificationDate :: ArticleMetadataStore -> Slug -> UTCTime -> ArticleMetadataStore
updateModificationDate (ArticleMetadataStore h) slug date =
  ArticleMetadataStore $ H.adjust updateCached slug h
  where
    updateCached cached =
      cached {cachedArticleMetadata = updateMetadata (cachedArticleMetadata cached)}
    updateMetadata metadata = metadata {articleModificationDate = Just date}

-- | Get the content of an article given its slug, if exists.
getArticleContent ::
  (MonadError e m, LiftArticleError e) =>
  ArticleMetadataStore ->
  Slug ->
  m Html
getArticleContent (ArticleMetadataStore h) slug =
  maybe (throwError . liftArticleError $ UnknownSlug slug) (pure . cachedArticleContent) $ H.lookup slug h

-- | Extract and convert the markup-formatted article into an HTML representation and cache it.
getCacheArticle ::
  (MonadIO m, MonadError e m, LiftArticleError e) =>
  ArticleMetadataStore ->
  ArticleMetadata ->
  m (Html, ArticleMetadataStore)
getCacheArticle (ArticleMetadataStore h) metadata = do
  raw <- liftIO $ do
    putStrLn $ "reading content of blog article " <> show slug <> " at path " <> path
    T.readFile path
  content <-
    case takeExtension path of
      ".md" -> convert Markdown $ markupToHtml Markdown raw
      ".org" -> convert Org $ markupToHtml Org raw
      ext -> throwError . liftArticleError $ UnknownMarkup ext
  let article = CachedArticle metadata content
      h' = H.insert slug article h
  pure (content, ArticleMetadataStore h')
  where
    path = articlePath metadata
    slug = articleSlug metadata
    convert mkp = either (throwError . liftArticleError . CannotTranslate mkp) pure
