module Wrapper (
    wrapper
  ) where

import Data.Text ( Text )
import Text.Blaze.Html5 as H hiding ( map )
import Text.Blaze.Html5.Attributes as A

wrapper :: String -> Html -> Html
wrapper t cont = do
    docTypeHtml $ do
      H.head $ do
        H.title (toHtml $ t)
        meta ! charset "utf-8"
        link ! rel "stylesheet" ! type_ "text/css" ! href "assets/css/style.css"
        link
          ! rel "stylesheet" 
          ! type_ "text/css"
          ! href "http://fonts.googleapis.com/css?family=Ubuntu:400,500&subset=latin,greek,latin-ext"
      H.body $ do
        H.menu menuPart
        section ! A.id "wrapper-content" $ cont

menuPart:: Html
menuPart =
    ul . sequence_ $ map li [homeLink,uploadLink,browseLink,contactLink,aboutLink]

homeLink :: Html
homeLink = navLink "/" "home"

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