module Markdown (
    markdownToHtml
  ) where

import Control.Monad.Error.Class (MonadError(..))
import Data.Default (def)
import Data.String (IsString(..))
import Data.Text (Text)
import Servant (ServantErr(..))
import Servant.Server (err500)
import Text.Blaze.Html5 as H
import Text.Pandoc (readMarkdown, writeHtml)

markdownToHtml :: (MonadError ServantErr m) => String -> m Html
markdownToHtml mkd = case readMarkdown def mkd of
  Left e -> throwError $ err500 { errBody = fromString $ "markdown compilation failed: " ++ show e }
  Right x -> pure (writeHtml def x)
