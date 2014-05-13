module Route.About (
    about
  ) where

import Control.Monad.Trans ( liftIO )
import Happstack.Server
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Markdown ( markdownToHtml )
import Wrapper ( wrapper )

about :: ServerPart Response
about = do
    cont <- liftIO . fmap markdownToHtml $
      readFile "assets/markdown/about.md"
    ok . toResponse . wrapper "About" $ section ! A.id "about-content" $ cont