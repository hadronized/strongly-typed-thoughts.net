module Route.Portfolio (
    portfolio
  ) where

import Control.Monad.Trans ( liftIO )
import Happstack.Server
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Markdown ( markdownToHtml )
import Wrapper ( wrapper )

portfolio :: ServerPart Response
portfolio = do
    cont <- liftIO . fmap markdownToHtml $
      readFile "assets/markdown/portfolio.md"
    ok . toResponse . wrapper "Portfolio" $
      section ! A.id "portfolio-content" $ cont
