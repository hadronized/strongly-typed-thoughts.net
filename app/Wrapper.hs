-- |This module exports an HTML wrapper function that can be used to unify how all pages look like.
module Wrapper (
    wrapper
  ) where

import Control.Monad (sequence_, void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (intersperse)
import Data.Text (Text)
import Data.Semigroup ((<>))
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Prelude hiding (div, id, head, span)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes hiding (span)

-- |Wrapper function that must be applied to any page’s content.
wrapper :: (MonadIO m) => Text -> Html -> m Html
wrapper t cont = do
    (year, _, _) <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime

    pure . docTypeHtml $ do
      head $ do
        H.title (toHtml t <> " – phaazon.net")
        meta ! charset "utf-8"
        meta ! name "viewport" ! content "width=device-width, initial-scale=1"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/bulma.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/font-awesome.min.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/index.css"
        link ! rel "icon" ! href "/static/imgs/tus.png"
        -- code hilighting
        link ! rel "stylesheet" ! href "/static/css/highlight/styles/gruvbox-dark.css"
        script ! src "/static/css/highlight/highlight.pack.js" $ ""
        script "hljs.initHighlightingOnLoad();"
        -- github activity
        link ! rel "stylesheet" ! href "//cdnjs.cloudflare.com/ajax/libs/octicons/2.0.2/octicons.min.css"
        link ! rel "stylesheet" ! href "/static/css/github-activity.css"
        script ! src "//cdnjs.cloudflare.com/ajax/libs/mustache.js/0.7.2/mustache.min.js" $ ""
        script ! src "/static/js/github-activity.js" $ ""
      body $ do
        menuPart t
        cont
        footerPart (fromIntegral year)

menuPart:: Text -> Html
menuPart t =
  section ! class_ "hero is-danger is-bold" $ do
    div ! class_ "hero-body" $ do
      div ! class_ "level is-mobile has-shadow" $ do
        div ! class_ "level-item" $
          p ! class_ "title" $ toHtml t

        div ! class_ "level-item" $ do
          a ! href "/" $
            span ! class_ "icon" $
              i ! class_ "fa fa-home" $ pure ()

        div ! class_ "level-item" $ do
          a ! href "https://git.phaazon.net" $
            span ! class_ "icon" $
              i ! class_ "fa fa-code-fork" $ pure ()
        div ! class_ "level-item" $ do
          a ! href "/portfolio" $
            span ! class_ "icon" $
              i ! class_ "fa fa-university" $ pure ()
        div ! class_ "level-item" $ do
          a ! href "/blog" $
            span ! class_ "icon" $
              i ! class_ "fa fa-pencil" $ pure ()
        div ! class_ "level-item" $ do
          a ! href "/browse" $
            span ! class_ "icon" $
              i ! class_ "fa fa-cloud-download" $ pure ()

footerPart :: Int -> Html
footerPart year = do
  footer ! class_ "footer" $ 
    div ! class_ "content has-text-centered" $ do
      p . sequence_ $ intersperse " "
        [
          a ! class_ "icon" ! href "https://github.com/phaazon" $
            i ! class_ "fa fa-github" $ pure ()
        , a ! class_ "icon" ! href "https://twitter.com/phaazon_" $
            i ! class_ "fa fa-twitter" $ pure ()
        , a ! class_ "icon" ! href "https://www.linkedin.com/in/dimitri-sabadie-97a9009b/" $
            i ! class_ "fa fa-linkedin-square" $ pure ()
        , a ! class_ "icon" ! href "https://soundcloud.com/phaazon" $
            i ! class_ "fa fa-soundcloud" $ pure ()
        , a ! class_ "icon" ! href "https://stackoverflow.com/users/1784267/phaazon" $
            i ! class_ "fa fa-stack-overflow" $ pure ()
        , a ! class_ "icon" ! href "/media/uploads/cv.pdf" $
            i ! class_ "fa fa-graduation-cap" $ pure ()
        ]
      p . sequence_ $ intersperse " · "
        [
          a ! href "http://haskell.org/" $ "Haskell"
        , a ! href "http://haskell-servant.readthedocs.io" $ "servant"
        , a ! href "http://bulma.io" $ "bulma"
        ]
      p $ void (toHtml $ "Copyright © 2014—" ++ show year ++ ", Dimitri Sabadie")
