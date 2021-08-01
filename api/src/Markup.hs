module Markup (
    Markup(..),
     markupToHtml
  ) where

import Data.Bifunctor (Bifunctor(..))
import Data.Default (def)
import Data.Text (Text)
import Text.Blaze.Html (Html)
import Text.Pandoc (pandocExtensions, readMarkdown, readOrg, readerExtensions, runPure, writeHtml5)

-- | Which markup to use.
-- Most of the time, the markup can be derived at call site based on the extension of the file the text comes from.
data Markup = Markdown | Org deriving (Eq, Show)

markupToHtml :: Markup -> Text -> Either String Html
markupToHtml mkp content = first show . runPure $ render mkp opts content >>= writeHtml5 def
  where
    render Markdown = readMarkdown
    render Org = readOrg
    opts = def { readerExtensions = pandocExtensions }
