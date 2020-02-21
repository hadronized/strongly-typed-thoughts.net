module Markdown (
    markdownToHtml
  , markdownToHtml_
  ) where

import Control.Monad.Error.Class (MonadError(..))
import Data.Default (def)
import Data.String.Conversions (convertString)
import Data.Text (Text)
import Servant.Server (ServerError(..), err500)
import Text.Blaze.Html (Html)
import Text.Pandoc (pandocExtensions, readMarkdown, readerExtensions, runPure, writeHtml5)

markdownToHtml :: (MonadError ServerError m) => Text -> m Html
markdownToHtml mkd = case runPure (readMarkdown opts mkd >>= writeHtml5 def) of
    Left e -> throwError $ err500 { errBody = "markdown compilation failed: " <> convertString (show e) }
    Right x -> pure x
  where
    opts = def { readerExtensions = pandocExtensions }

markdownToHtml_ :: Text -> Either String Html
markdownToHtml_ mkd = case runPure (readMarkdown opts mkd >>= writeHtml5 def) of
    Left e -> Left (show e)
    Right x -> Right x
  where
    opts = def { readerExtensions = pandocExtensions }
