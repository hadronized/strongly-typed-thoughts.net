module Wrapper (
    wrapper
  ) where

import Control.Monad ( void )
import Data.Text ( Text )
import Text.Blaze.Html5 as H hiding ( map )
import Text.Blaze.Html5.Attributes as A

wrapper :: String -> Html -> Html
wrapper t cont = do
    docTypeHtml $ do
      H.head $ do
        H.title (toHtml $ t)
        meta ! charset "utf-8"
        link ! rel "stylesheet" ! type_ "text/css" ! href "assets/css/style.css"
        link ! rel "icon" ! href "static/img/tus.png"
        link
          ! rel "stylesheet" 
          ! type_ "text/css"
          ! href "http://fonts.googleapis.com/css?family=Ubuntu:400,500&subset=latin,greek,latin-ext"
      H.body $ do
        H.menu menuPart
        section ! A.id "wrapper-content" $ cont
        footer footerContent

menuPart:: Html
menuPart =
    ul . sequence_ $ map li
      [
        homeLink
      , portfolioLink
      , blogLink
      , uploadLink
      , browseLink
      , contactLink
      , aboutLink
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

contactLink :: Html
contactLink = navLink "/contact" "contact"

aboutLink :: Html
aboutLink = navLink "/about" "about"

navLink :: AttributeValue -> Text -> Html
navLink url t = a ! href url $ toHtml t

footerContent :: Html
footerContent = do
    void "Powered by "
    a ! href "http://happstack.com" $ "Happstack server"
    void ", Copyright 2014, Dimitri Sabadie"
