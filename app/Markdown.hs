module Markdown (
    markdownToHtml
  ) where

import Data.Default (def)
import Data.Text (Text)
import Text.Blaze.Html5 as H
import Text.Pandoc (readMarkdown, writeHtml)

markdownToHtml :: String -> Html
markdownToHtml mkd = case readMarkdown def mkd of
  Left _ -> ""
  Right x -> writeHtml def x
