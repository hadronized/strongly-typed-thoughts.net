module Route.Home where

import Control.Monad.Trans ( liftIO )
import Happstack.Server
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Markdown
import Wrapper

-- nothing special to do in there, just get the markdown version of the home
-- page
home :: ServerPart Response
home = do
    cont <- liftIO . fmap markdownToHtml $
      readFile "assets/markdown/home.md"
    ok . toResponse . wrapper "Home" $ section ! A.id "home-content" $ cont