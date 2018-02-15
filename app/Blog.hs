{-# LANGUAGE DataKinds #-}

module Blog (
  BlogApi,
  blog 
) where

import Servant (Get)
import Servant.HTML.Blaze (HTML)
import Servant.Server (Server)
import Text.Blaze.Html5 hiding (style)
import Text.Blaze.Html5.Attributes hiding (content, icon, name, span)

import Wrapper (wrapper)

type BlogApi = Get '[HTML] Html

blog :: Server BlogApi 
blog = wrapper "Blog" $ do
  section ! class_ "container section content" $ do
    h1 ! class_ "title" $ do
      b "Dimitri Sabadie"
      " blog"
    h2 ! class_ "subtitle" $ em $ "Functional programming, graphics, demoscene and more!"
    hr
    p $ do
      "This is my blog. I talk about functional programming, graphics, demoscene, optimization "
      "and many other topics!"
    blockquote $ do
      "It is intentional that no comment can be written by readers to prevent flooding, scams "
      "and spamming."

