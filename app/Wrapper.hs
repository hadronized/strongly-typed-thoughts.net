-- |This module exports an HTML wrapper function that can be used to unify how all pages look like.
module Wrapper (
    wrapper
  ) where

import Control.Monad (sequence_, void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (intersperse)
import Data.Text (Text)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Text.Blaze.Html5 as H hiding (map)
import Text.Blaze.Html5.Attributes hiding (span)
import Prelude hiding (div, id, head, span)

-- |Wrapper function.
wrapper :: (MonadIO m) => Text -> Html -> m Html
wrapper t cont = do
    (year, _, _) <- liftIO $ fmap (toGregorian . utctDay) getCurrentTime

    pure . docTypeHtml $ do
      head $ do
        H.title (toHtml $ t)
        meta ! charset "utf-8"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/bulma.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/font-awesome.min.css"
        link ! rel "stylesheet" ! type_ "text/css" ! href "/static/css/index.css"
        link ! rel "icon" ! href "/static/img/tus.png"
      body $ do
        menuPart t
        cont
        footerPart (fromIntegral year)

menuPart:: Text -> Html
menuPart t =
  section ! class_ "hero is-danger is-bold" $ do
    div ! class_ "hero-body" $ do
      div ! class_ "container" $ do
        nav ! class_ "nav has-shadow" $ do
          div ! class_ "nav-left" $
            a ! class_ "nav-item is-tab" ! href "#" $ toHtml t

          --div ! class_ "nav-center" $ do

          div ! class_ "nav-right" $ do
            a ! class_ "nav-item is-tab" ! href "/" $
              span ! class_ "icon" $
                i ! class_ "fa fa-home" $ pure ()
            a ! class_ "nav-item is-tab" ! href "/portfolio" $
              span ! class_ "icon" $
                i ! class_ "fa fa-university" $ pure ()
            a ! class_ "nav-item is-tab" ! href "/blog" $
              span ! class_ "icon" $
                i ! class_ "fa fa-pencil" $ pure ()
            -- a ! class_ "nav-item is-tab" ! href "/upload" $
            --   span ! class_ "icon" $
            --     i ! class_ "fa fa-cloud-upload" $ pure ()
            a ! class_ "nav-item is-tab" ! href "/browse" $
              span ! class_ "icon" $
                i ! class_ "fa fa-cloud-download" $ pure ()

footerPart :: Int -> Html
footerPart year = do
  footer ! class_ "footer" $ 
    div ! class_ "container" $ do
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
