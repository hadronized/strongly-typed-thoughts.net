-- |This module exports an HTML wrapper function that can be used to unify how all pages look like.
module Wrapper (
    wrapper
  ) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Text (Text)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Text.Blaze.Html5 as Blaze ((!), AttributeValue, Html, a, body, docTypeHtml, footer, head, li,
                                  link, menu, meta, section, title, toHtml, ul)
import Text.Blaze.Html5.Attributes as Blaze (charset, id, href, rel, type_)

-- |Wrapper function.
wrapper :: (MonadIO m) => Text -> Html -> m Html
wrapper t cont = do
    (year, _, _) <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime

    pure . docTypeHtml $ do
      Blaze.head $ do
        title (toHtml $ t)
        meta ! charset "utf-8"
        link ! rel "stylesheet" ! type_ "text/css" ! href "static/css/reset.css"
        link ! rel "icon" ! href "static/img/tus.png"
        link
          ! rel "stylesheet" 
          ! type_ "text/css"
          ! href "http://fonts.googleapis.com/css?family=Ubuntu:400,500&subset=latin,greek,latin-ext"
      body $ do
        menu menuPart
        section ! Blaze.id "wrapper-content" $ cont
        footer (footerContent $ fromIntegral year)

menuPart:: Html
menuPart =
    ul . sequence_ $ map li -- TODO: test with ul . concatMap $ map li instead
      [
        homeLink
      , portfolioLink
      , blogLink
      , uploadLink
      , browseLink
      ]

homeLink :: Html
homeLink = navLink "/" "home"

portfolioLink :: Html
portfolioLink = navLink "/portfolio" "portfolio"

blogLink :: Html
blogLink = navLink "http://phaazon.blogspot.fr" "blog"

uploadLink :: Html
uploadLink = navLink "/upload" "upload"

browseLink :: Html
browseLink = navLink "/browse" "browse"

navLink :: AttributeValue -> Text -> Html
navLink url t = a ! href url $ toHtml t

footerContent :: Int -> Html
footerContent year = do
    void "Powered by "
    a ! href "http://happstack.com" $ "Happstack server"
    void (toHtml $ ", Copyright 2014—" ++ show year ++ ", Dimitri Sabadie")
