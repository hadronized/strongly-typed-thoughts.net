{-# LANGUAGE DataKinds #-}

module Portfolio (
  PortfolioApi,
  portfolio
) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Data.Text (Text)
import Prelude hiding (div)
import Servant (Get)
import Servant.HTML.Blaze (HTML)
import Servant.Server (Server)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import Markdown (markdownToHtml)
import Wrapper (wrapper)

type PortfolioApi = Get '[HTML] Html

category :: Html -> Html -> AttributeValue -> Html
category name subname color = do
  section ! class_ ("section hero content is-right " <> color) $ do
    h1 ! class_ "title" $ name
    h2 ! class_ "subtitle is-small" $ subname

entry :: Maybe AttributeValue -> Html -> Html -> Html -> Html -> Html -> Html
entry imgSrc prodName prodQuickInfo icons prodExtraInfo content = do
  section ! class_ "section" $ do
    div ! class_ "card" $ do
      div ! class_ "card-content" $ do
        div ! class_ "media" $ do
          for_ imgSrc $ \imgSrc' ->
            div ! class_ "media-left" $ do
              figure ! class_ "image is-256x256" $
                img ! alt "Image" ! src ("/static/imgs/" <> imgSrc')
          div ! class_ "media-content" $ do
            div ! class_ "columns" $ do
              div ! class_ "column" $ do
                p ! class_ "title is-4" $ prodName
                p ! class_ "subtitle is-6" $ prodQuickInfo
                icons
              div ! class_ "column" $ prodExtraInfo
        div ! class_ "content" $ content

portfolio :: Server Portfolio
portfolio = wrapper "Portfolio" $ do
  section ! class_ "container section content" $ do
    h1 ! class_ "title" $ "Portfolio"
    hr
    p "Here’s a non-comprehensive list of all personal or school projects I worked or I’m working on. I chose to show you those projects because I believe they’re the most representative part of my work on my spare time. Feel free to test and give me your feedback!"
    p "The portfolio is organized by:"
    ul $ do
      li $ a ! href "#demoscene" $ "demoscene"
      li $ a ! href "#community" $ "community"
      li $ a ! href "#contributions" $ "contributions"
      li $ a ! href "#school" $ "school"
    p $ do
      void "If you want a more comprehensive list of what I do, feel free to visit my "
      a ! href "https://github.com/phaazon" $ "github page"

  category "Demoscene" "Artsy, size-limited, mathematical, underground" "is-info"
  entryCeleriRemoulade

entryCeleriRemoulade :: Html
entryCeleriRemoulade =
    entry (Just "celeri_remoulade.png")
          "Céleri Rémoulade"
          "Evoke 2016"
          icons
          extra
          content

  where
    icons = do
      a ! class_ "icon" ! href "https://github.com/phaazon/celeri-remoulade" $
        i ! class_ "fa fa-github" $ pure ()
      a ! class_ "icon" ! href "https://demozoo.org/productions/161887" $
        i ! class_ "fa fa-microchip" $ pure ()
      a ! class_ "icon" ! href "http://www.pouet.net/prod.php?which=67966" $
        i ! class_ "fa fa-globe" $ pure ()
    extra = do
      p ! class_ "subtitle is-6" $ "PC Demo"
      p ! class_ "subtitle is-6" $ "Ranked 14th/18"
    content = do
      p "My third demoscene production – and first PC demo release. Released at Evoke 2016 (Köln, Germany), and ranked 14th / 18."
      p $ do
        void "It is a one-man production that I wrote entirely in "
        b "Rust"
        void " using my "
        a ! href "https://github.com/phaazon/luminance-rs" $ "luminance"
        void " graphics framwork, OpenAL and libvorbis"
