{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Blog (
    BlogApi
  , BlogEntryMapping
  , blog
  , defaultBlogEntryMapping
  , refreshBlog
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Aeson (FromJSON)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as H
import Data.List (stripPrefix)
import Data.Maybe (fromJust)
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Time (UTCTime)
import Data.Traversable (for)
import Data.Yaml (ParseException, decodeFileEither)
import GHC.Generics (Generic)
import Prelude hiding (div, span)
import Servant (Get)
import Servant.HTML.Blaze (HTML)
import Servant.Server (Server)
import Text.Blaze.Html5 hiding (style)
import Text.Blaze.Html5.Attributes hiding (content, for, icon, name, span)

import Markdown (markdownToHtml_)
import Wrapper (wrapper)

type BlogApi = Get '[HTML] Html

-- The blog entries configuration.
newtype BlogEntryManifest = BlogEntryManifest {
    getEntries :: [BlogEntry]
  } deriving (Eq, FromJSON, Show)

-- A blog entry.
--
-- It has metadata so that we know when it was written, modified, etc.
data BlogEntry = BlogEntry {
    -- Name of the entry. It’s used in listings, for instance.
    blogEntryName :: Text
    -- Path to the Markdown-formatted article.
  , blogEntryPath :: FilePath
    -- Date when the article was published.
  , blogEntryPublishDate :: UTCTime
    -- Slug used in URL to refer to that article.
  , blogEntrySlug :: Text
  } deriving (Eq, Generic, Show)

instance FromJSON BlogEntry where

-- Generate the Html from a BlogEntry.
blogEntryToHtml :: (MonadIO m) => BlogEntry -> m Html
blogEntryToHtml entry = do
  mkd <- liftIO . T.readFile $ blogEntryPath entry
  case markdownToHtml_ mkd of
    Left e -> liftIO (print e) >> pure ""
    Right x -> pure x

-- Read the blog entry manifest from a file.
readBlogEntryManifest :: (MonadIO m) => FilePath -> m (Either ParseException BlogEntryManifest)
readBlogEntryManifest = liftIO . decodeFileEither

-- The internal structure that maps the HTML to a given blog entry.
newtype BlogEntryMapping = BlogEntryMapping {
    blogEntryMap :: HashMap Text (BlogEntry, Html)
  }

defaultBlogEntryMapping :: BlogEntryMapping
defaultBlogEntryMapping = BlogEntryMapping mempty

-- Read all the articles and return the HTML representation. Return a string description errors
-- if any occurred.
refreshBlog :: (MonadIO m) => FilePath -> TVar BlogEntryMapping -> m ()
refreshBlog manifestPath blogEntryMapping = do
  manif <- readBlogEntryManifest manifestPath
  case manif of
    Left e -> liftIO (print e)
    Right manif' -> do
      -- we render all entries and regenerate the hashmap
      entryMap <- fmap (BlogEntryMapping . H.fromList) . for (getEntries manif') $ \entry -> do
        content <- blogEntryToHtml entry
        pure (blogEntrySlug entry, (entry, content))
      liftIO . atomically $ writeTVar blogEntryMapping entryMap

blog :: TVar BlogEntryMapping -> Server BlogApi 
blog blogEntryMapping = do
  entries <- liftIO (readTVarIO blogEntryMapping)
  wrapper "Blog" $ do
    section ! class_ "container section content" $ do
      h1 ! class_ "title" $ do
        b "Dimitri Sabadie"
        " blog"
      h2 ! class_ "subtitle" $ em $ "Functional programming, graphics, demoscene and more!"
      hr
      p $ do
        "This is my blog. I talk about functional programming, graphics, demoscene, optimization "
        "and many other topics!"
      blockquote $ do
        "It is intentional that no comment can be written by readers to prevent flooding, scams "
        "and spamming."
      hr
      traverse_ blogListing (blogEntryMap entries)

blogListing :: (BlogEntry, Html) -> Html
blogListing (entry, content) = do
  div ! class_ "level" $ do
    span ! class_ "level-left" $ do
      span ! class_ "level-item" $
        a ! href (toValue $ blogEntrySlug entry) $ toHtml (blogEntryName entry)
    span ! class_ "level-right" $ do
      span ! class_ "level-item" $ em $ "on " <> toHtml (show $ blogEntryPublishDate entry)

