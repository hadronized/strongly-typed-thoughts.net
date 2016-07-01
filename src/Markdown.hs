module Markdown (
    markdownToHtml
  ) where

import Data.Default ( def )
import Text.Blaze.Html5 as H
import Text.Pandoc ( readMarkdown, writeHtml )

markdownToHtml :: String -> Html
markdownToHtml mkd = case readMarkdown def mkd of
  Left _ -> ""
  Right a -> writeHtml def a
