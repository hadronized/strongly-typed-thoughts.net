{-# LANGUAGE DataKinds #-}

module Feed (
    FeedApi
  , feed
  ) where

import Control.Concurrent.STM.TVar (TVar, readTVarIO)
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.HashMap.Strict as H
import Data.List (intersperse)
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
feed mapping = liftIO . fmap (rssFeed . getItems) $ readTVarIO mapping
  where
    getItems = map (rssItem . fst) . H.elems . blogEntryMap

urlBase :: Text
urlBase = "https://phaazon.net"

urlBlogBase :: Text
urlBlogBase = urlBase <> "/blog"

rssFeed :: [RSSItem] -> RSS
rssFeed items = (nullRSS "phaazon.net" urlBase) { rssChannel = rssChan items }

rssChan :: [RSSItem] -> RSSChannel
rssChan items = (nullChannel "phaazon.net blog" urlBlogBase) { rssItems = items }

rssItem :: BlogEntry -> RSSItem
rssItem entry =
    (nullItem $ blogEntryName entry) {
      rssItemLink = Just (urlBlogBase <> "/" <> blogEntrySlug entry),
      rssItemDescription = Just (T.concat . intersperse ","  $ blogEntryTags entry),
      rssItemAuthor = Just "Dimitri 'phaazon' Sabadie <dimitri.sabadie@gmail.com>",
      rssItemPubDate = Just (pack . formatTime defaultTimeLocale fmtStr $ blogEntryPublishDate entry)
    }
  where
    fmtStr = "%a, %d %b %Y %H:%M:%S GMT"
