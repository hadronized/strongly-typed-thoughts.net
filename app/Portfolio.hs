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
import Prelude hiding (div, span)
import Servant (Get)
import Servant.HTML.Blaze (HTML)
import Servant.Server (Server)
import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes hiding (icon, span)

import Markdown (markdownToHtml)
import Wrapper (wrapper)

type PortfolioApi = Get '[HTML] Html

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
  entryOutline2017Invitation
  entrySpectra
  entryCeleriRemoulade
  entryLuminanceRust
  entryLuminanceHaskell
  entryQuaazar
  entryIonosphere

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

icon :: AttributeValue -> AttributeValue -> Html
icon url faIcon =
  a ! class_ "icon" ! href url $
    i ! class_ ("fa " <> faIcon) $ pure ()

entryOutline2017Invitation :: Html
entryOutline2017Invitation =
    entry (Just "outline_2017_invitation.png")
          "Outline 2017 Invitation"
          "Revision 2017"
          icons
          extra
          content
  where
    icons = do
      icon "https://www.youtube.com/watch?v=OemyLQbDTSk" "fa-youtube"
      icon "https://github.com/phaazon/outline-2017-invitro" "fa-github"
      icon "https://demozoo.org/productions/170916" "fa-microchip"
      icon "https://www.pouet.net/prod.php?which=69698" "fa-globe"
    extra = do
      p ! class_ "subtitle is-6" $ "PC Demo"
      p ! class_ "subtitle is-6" $ "Ranked 19th/26"
    content = do
      p $ do
        void "Two weeks before Revision 2017, I was offered the opportunity to write the Outline 2017"
        void " official invitation. I teamed up with some DESiRE members (graphics, music and"
        void " direction, mostly) and I was the sole programmer, writing the demo in Rust with my"
        void " spectra demoscene framework."
      p $ do
        void "The overall demo is not impressive and doesn’t look that good, but I’m really proud of"
        void " it, because it was made in only two weeks and I tested a lot of spectra’s features in"
        void " the time of writing the production."

entrySpectra :: Html
entrySpectra =
    entry Nothing
          "Spectra"
          "2016–"
          icons
          extra
          content
  where
    icons = do
      icon "https://github.com/phaazon/spectra" "fa-github"
      icon "https://crates.io/crates/spectra" "fa-gear"
      icon "https://docs.rs/spectra" "fa-search"
    extra = p ! class_ "subtitle is-6" $ "Rust demoscene framework"
    content = do
      p "Spectra is my Rust demoscene framework. It features a lot of cool features, among:"
      ul $ do
        li "resource hot reloading"
        li "splines, for animation or anything that requires interpolation"
        li "timelines"
        li "EDSL for shading language"
        li "mesh, models, materials, camera, lights, etc."
        li "easy setup"
        li "and a lot of other cools features"
      p $ do
        void "Currently, the project is a massive work in progress."

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
      icon "https://www.youtube.com/watch?v=pYqZS1C_7PU" "fa-youtube"
      icon "https://github.com/phaazon/celeri-remoulade" "fa-github"
      icon "https://demozoo.org/productions/161887" "fa-microchip"
      icon "http://www.pouet.net/prod.php?which=67966" "fa-globe"
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

entryLuminanceRust :: Html
entryLuminanceRust =
    entry Nothing
          "Luminance"
          "2016"
          icons
          extra
          content
  where
    icons = do
      icon "https://github.com/phaazon/luminance-rs" "fa-github"
      icon "https://crates.io/crates/luminance" "fa-gear"
      icon "https://docs.rs/luminance" "fa-search"
    extra = p ! class_ "subtitle is-6" $ "Rust graphics framework"
    content = do
      p $ do
        void "I learned "
        a ! href "https://www.rust-lang.org" $ "Rust"
        void " by March, 2016. Fallen in love with that language, which is very similar to Haskell with"
        void " the control over the memory and predictive performance as an extra argument, I decided to"
        void " jump in and migrate as fast as the very next day my Haskell luminance framework. The"
        void " migration took me some time, because Rust doesn’t directly match to Haskell. For instance,"
        void " Haskell has more powerful abstraction tools, forcing me to adapt the abstractions and hack"
        void " around for several hours with Rust’s constructs. As an example, Haskell’s "
        em "existential quantification"
        void ", which doesn’t exist in Rust – I mapped it by trait object, btw."
      p $ do
        void "The project starts to be mature, thanks to all the code I wrote in Haskell (36 releases),"
        void " and that I’m writing a demoscene production with it. So I’m testing luminance in real"
        void " conditions, changing the interface on the fly to make it more comfortable and benchmarking."

entryLuminanceHaskell :: Html
entryLuminanceHaskell =
    entry Nothing
          "Luminance"
          "2015"
          icons
          extra
          content
  where
    icons =
      icon "https://github.com/phaazon/luminance" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "Haskell graphics framework"
    content = do
      p $ do
        void "luminance is born from the ashes of quaazar. The aims of luminance are simple: it’s a"
        void " wrapper over graphics technologies (OpenGL) designed to be stateless, type-safe and simple."
      p $ do
        void "I think that project was the most followed of mine. I got feedback from people, a lot"
        void " of talks on reddit and IRC; I was asked for help by people wanting to hear from the"
        void " experience of “making something unsafe and stateful safe and stateless”, because"
        void " that’s exactly what luminance is all about."
      p "After more than 36 releases, I decided to migrate the framework into its Rust version."

entryQuaazar :: Html
entryQuaazar =
    entry (Just "quaazar.png")
          "Quaazar"
          "2015"
          icons
          extra
          content
  where
    icons = icon "https://github.com/phaazon/quaazar" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "Haskell demoscene framework"
    content = do
      p $ do
        "quaazar was my first attempt at serious graphics programming in Haskell for demoscene"
        " purposes. After coming to the realization that I was building something way too much"
        " generic, I decided to discontinue the project and extract a sub part of it (luminance)."

entryIonosphere :: Html
entryIonosphere =
    entry Nothing
        "Ionosphere"
        "Outline 2014"
        icons
        extra
        content
  where
    icons = do
      icon "https://soundcloud.com/phaazon/ionosphere" "fa-soundcloud"
      icon "https://demozoo.org/music/112594" "fa-microchip"
    extra = do
      p ! class_ "subtitle is-6" $ "Newschool / Streaming Music"
      p ! class_ "subtitle is-6" $ "Ranked 15th/18"
    content = do
      p $ do
        "Ionosphere is my very first attempt at music making. Back from VIP2014, I decided to go"
        " wild, buy Renoise 3.0 and make a tiny but lovely song in 4 hours."

entryHeatStation :: Html
entryHeatStation =
    entry (Just "heat_station.png")
          "Heat Station"
          "Evoke 2013"
          icons
          extra
          content
  where
    icons = do
      icon "http://www.youtube.com/watch?v=aU30N9YSpBY" "fa-youtube"
      icon "https://github.com/phaazon/heatstation" "fa-github"
      icon "http://www.pouet.net/prod.php?which=61729" "fa-globe"
    extra = do
      p ! class_ "subtitle is-6" $ "PC Intro 64k (Windows / Linux)"
      p ! class_ "subtitle is-6" $ "Ranked 4th/4"
    content = do
      p $ do
        "Heat Station is my second 64k intro, released at Evoke 2013. I wrote it in C++ with"
        " skyoralis, my demoscene 3D realtime engine at that time. It was a test-release for my"
        " engine, and I had to rush the Windows port, so take it as-is!"
      p $ "It ranked 4th/4, behind Farbrausch, Inque and Stroboholics."
