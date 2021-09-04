{-# LANGUAGE OverloadedStrings #-}

module Feed
  ( rssFeed,
    rssItem,
  )
where

import Blog (ArticleMetadata (..), Slug (..), articleLastModificationDate)
import Data.List (intersperse)
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import GHC.TypeLits (symbolVal)
import Text.RSS.Syntax (RSS (..), RSSChannel (..), RSSItem (..), nullChannel, nullItem, nullRSS)

urlHost :: Text
urlHost = "https://phaazon.net"

urlAPI :: Text
urlAPI = urlHost <> "/api"

urlBlog :: Text
urlBlog = urlAPI <> "/blog"

rssFeed :: UTCTime -> [RSSItem] -> RSS
rssFeed lastUpdateTime items = (nullRSS "phaazon.net" urlHost) {rssChannel = rssChan (Just lastUpdateTime) items}

rssChan :: Maybe UTCTime -> [RSSItem] -> RSSChannel
rssChan lastUpdateTime items =
  (nullChannel "phaazon.net blog" urlBlog)
    { rssItems = items,
      rssLastUpdate = fmap dateToText lastUpdateTime
    }

rssItem :: ArticleMetadata -> RSSItem
rssItem article =
  (nullItem $ articleName article)
    { rssItemLink = Just $ urlBlog <> "/" <> unSlug (articleSlug article),
      rssItemDescription = Just (T.concat . intersperse "," $ articleTags article),
      rssItemAuthor = Just "Dimitri 'phaazon' Sabadie <dimitri.sabadie@gmail.com>",
      rssItemPubDate = Just (dateToText $ articleLastModificationDate article)
    }

dateToText :: UTCTime -> Text
dateToText = pack . formatTime defaultTimeLocale fmtStr
  where
    fmtStr = "%a, %d %b %Y %H:%M:%S GMT"
