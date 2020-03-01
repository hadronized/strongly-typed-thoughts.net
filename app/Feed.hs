{-# LANGUAGE DataKinds #-}

module Feed (
    FeedApi
  , feed
  ) where

import Control.Concurrent.STM.TVar (TVar, readTVarIO)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.HashMap.Strict as H
import Data.List (intersperse)
import Data.Time (UTCTime)
import Data.Text (Text, pack)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Text.RSS.Syntax (RSS(..), RSSChannel(..), RSSItem(..), nullChannel, nullItem, nullRSS)
import qualified Data.Text as T
import Servant (Get)
import Servant.Server (Server)

import Blog (BlogEntry(..), BlogEntryMapping(..))
import XML (XML)

type FeedApi = Get '[XML] RSS

feed :: TVar BlogEntryMapping -> Server FeedApi
feed blogEntryMappingTVar = do
    blogEntryMapping <- liftIO (readTVarIO blogEntryMappingTVar)
    let lastUpdateTime = blogLastUpdateDate blogEntryMapping
    pure . rssFeed lastUpdateTime . getItems $ blogEntryMap blogEntryMapping
  where
    getItems = map (rssItem . fst) . H.elems

urlBase :: Text
urlBase = "https://phaazon.net"

urlBlogBase :: Text
urlBlogBase = urlBase <> "/blog"

rssFeed :: Maybe UTCTime -> [RSSItem] -> RSS
rssFeed lastUpdateTime items = (nullRSS "phaazon.net" urlBase) { rssChannel = rssChan lastUpdateTime items }

rssChan :: Maybe UTCTime -> [RSSItem] -> RSSChannel
rssChan lastUpdateTime items = (nullChannel "phaazon.net blog" urlBlogBase) {
    rssItems = items,
    rssLastUpdate = fmap dateToText lastUpdateTime
  }

rssItem :: BlogEntry -> RSSItem
rssItem entry =
  (nullItem $ blogEntryName entry) {
    rssItemLink = Just (urlBlogBase <> "/" <> blogEntrySlug entry),
    rssItemDescription = Just (T.concat . intersperse ","  $ blogEntryTags entry),
    rssItemAuthor = Just "Dimitri 'phaazon' Sabadie <dimitri.sabadie@gmail.com>",
    rssItemPubDate = Just (dateToText $ blogEntryPublishDate entry)
  }

dateToText :: UTCTime -> Text
dateToText = pack . formatTime defaultTimeLocale fmtStr
  where
    fmtStr = "%a, %d %b %Y %H:%M:%S GMT"
