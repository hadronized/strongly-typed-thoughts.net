-- Blog articles.
module Blog
  ( ArticleMetadata (..),
    ArticleMetadataStore,
    storeFromMetadata,
    metadataArticles,
    articleContent,
    getCacheArticle,
    readMetadata,
    updateModificationDate,
    LiftArticleError (..),
    ArticleError (..),
  )
where

import Control.Monad (foldM)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, genericParseJSON, genericToEncoding, genericToJSON)
import Data.Bifunctor (Bifunctor (..))
import qualified Data.ByteString.Lazy as BS
import qualified Data.HashMap.Strict as H
import qualified Data.Text.IO as T
import Data.Text.Lazy (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import JSON (jsonOptions)
import Markup (Markup (..), markupToHtml)
import System.FilePath (takeExtension)
import Text.Blaze.Html.Renderer.Text (renderHtml)

-- | Type of error that can occur while dealing with blog articles.
data ArticleError
  = CannotDecodeMetadata String
  | CannotTranslate Markup String
  | UnknownMarkup String
  deriving (Eq, Show)

class LiftArticleError e where
  liftArticleError :: ArticleError -> e

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
    articleSlug :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON ArticleMetadata where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ArticleMetadata where
  toEncoding = genericToEncoding jsonOptions
  toJSON = genericToJSON jsonOptions

-- A cached article is the combination of metadata about the article as well as the content of the article optionally
-- loaded.
data CachedArticle = CachedArticle
  { cachedArticleMetadata :: Maybe ArticleMetadata,
    cachedArticleContent :: Text
  }
  deriving (Generic)

instance FromJSON CachedArticle where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON CachedArticle where
  toEncoding = genericToEncoding jsonOptions
  toJSON = genericToJSON jsonOptions

-- Article metadata store.
newtype ArticleMetadataStore
  = ArticleMetadataStore (H.HashMap FilePath CachedArticle)
  deriving (Generic)

instance FromJSON ArticleMetadataStore where
  parseJSON = genericParseJSON jsonOptions

instance ToJSON ArticleMetadataStore where
  toEncoding = genericToEncoding jsonOptions
  toJSON = genericToJSON jsonOptions

readMetadata :: (MonadIO m, MonadError e m, LiftArticleError e) => FilePath -> m [ArticleMetadata]
readMetadata path = do
  content <- liftIO (BS.readFile path)
  either (throwError . liftArticleError . CannotDecodeMetadata) pure $ eitherDecode content

storeFromMetadata :: (MonadIO m, MonadError e m, LiftArticleError e) => [ArticleMetadata] -> m ArticleMetadataStore
storeFromMetadata =
  foldM (\h metadata -> fmap snd . getCacheArticle h $ articlePath metadata) (ArticleMetadataStore H.empty)

-- Get a listing of articles metadata.
metadataArticles :: ArticleMetadataStore -> [(FilePath, Maybe ArticleMetadata)]
metadataArticles (ArticleMetadataStore h) = map (second cachedArticleMetadata) $ H.toList h

-- Update last modification date of an article.
updateModificationDate :: ArticleMetadataStore -> FilePath -> UTCTime -> ArticleMetadataStore
updateModificationDate (ArticleMetadataStore h) path date =
  ArticleMetadataStore $ H.adjust updateCached path h
  where
    updateCached cached =
      cached {cachedArticleMetadata = fmap updateMetadata (cachedArticleMetadata cached)}
    updateMetadata metadata = metadata {articleModificationDate = Just date}

-- | Given a filepath representing an article, grabs it and returns its content along with the store, optionally altered
-- if loading the file was required.
articleContent ::
  (MonadIO m, MonadError e m, LiftArticleError e) =>
  ArticleMetadataStore ->
  FilePath ->
  m (Text, ArticleMetadataStore)
articleContent store@(ArticleMetadataStore h) path =
  case H.lookup path h of
    Just article -> pure (cachedArticleContent article, store)
    Nothing -> getCacheArticle store path

-- | Extract and convert the markup-formatted article into an HTML representation and cache it.
getCacheArticle ::
  (MonadIO m, MonadError e m, LiftArticleError e) =>
  ArticleMetadataStore ->
  FilePath ->
  m (Text, ArticleMetadataStore)
getCacheArticle (ArticleMetadataStore h) path = do
  raw <- liftIO $ do
    putStrLn $ "  · reading content of blog article at " <> path
    T.readFile path
  content <-
    case takeExtension path of
      ".md" -> convertError Markdown $ markupToHtml Markdown raw
      ".org" -> convertError Org $ markupToHtml Org raw
      ext -> throwError . liftArticleError $ UnknownMarkup ext
  let article = CachedArticle Nothing content
      h' = H.insert path article h
  pure (content, ArticleMetadataStore h')
  where
    convertError mkp = either (throwError . liftArticleError . CannotTranslate mkp) (pure . renderHtml)
