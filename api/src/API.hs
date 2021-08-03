-- | API endpoints and handlers.
module API
  ( API,
  )
where

import Data.Text (Text)
import Servant.API (Capture, Get, PlainText, Raw, (:<|>) (..), (:>))
import Servant.HTML.Blaze (HTML)
import Text.Blaze.Html (Html)
import Text.RSS.Syntax (RSS)
import XML (XML)

type API =
  "media" :> Raw
    :<|> "pub" :> Raw
    :<|> "static" :> Raw
    :<|> "gpg" :> Get '[PlainText] Text
    :<|> "blog" :> BlogAPI

type BlogAPI =
  "feed" :> FeedApi
    :<|> BlogArticleAPI

type FeedApi = Get '[XML] RSS

type BlogArticleAPI =
  Get '[HTML] Html
    :<|> Capture "slug" Text :> Get '[HTML] Html
