module Route.Contact (
    contact
  ) where

import Control.Monad.Trans ( liftIO )
import Happstack.Server
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

import Markdown ( markdownToHtml )
import Wrapper ( wrapper )

contact :: ServerPart Response
contact = do
    cont <- liftIO . fmap markdownToHtml $
      readFile "assets/markdown/contact.md"
    ok . toResponse . wrapper "Contact" $ section ! A.id "contact-content" $ cont