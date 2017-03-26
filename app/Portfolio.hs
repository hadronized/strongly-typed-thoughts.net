{-# LANGUAGE DataKinds #-}

module Portfolio (
  Portfolio,
  portfolio
) where

import Control.Monad.IO.Class (MonadIO(..))
import Servant (Get, ServantErr(..))
import Servant.HTML.Blaze (HTML)
import Servant.Server (Server)
import Text.Blaze.Html5 (Html, (!), section)
import qualified Text.Blaze.Html5.Attributes as A (id)

import Markdown (markdownToHtml)
import Wrapper (wrapper)

type Portfolio = Get '[HTML] Html

portfolio :: Server Portfolio 
portfolio = do
  content <- liftIO (readFile "static/content/portfolio.md") >>= markdownToHtml
  wrapper "Portfolio" (section ! A.id "portfolio-content" $ content)
