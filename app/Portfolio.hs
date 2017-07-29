{-# LANGUAGE DataKinds #-}

module Portfolio (
  PortfolioApi,
  portfolio
) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Char (toLower)
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Data.String (IsString(..))
import Prelude hiding (div, id, span)
import Servant (Get)
import Servant.HTML.Blaze (HTML)
import Servant.Server (Server)
import Text.Blaze.Html5 hiding (style)
import Text.Blaze.Html5.Attributes hiding (content, icon, name, span)

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
      li $ a ! href "#school" $ "school"
    p $ do
      void "If you want a more comprehensive list of what I do, feel free to visit my "
      a ! href "https://github.com/phaazon" $ "github page"

  category "Demoscene" "Artsy, size-limited, mathematical, underground" "is-info"
  entryOutline2017Invitation
  entrySpectra
  entryCeleriRemoulade
  entryLuminanceRust
  entryLuminanceHaskell
  entryQuaazar
  entryIonosphere
  entryHeatStation
  entryLR2LR
  entrySkyoralis

  category "Community" "Personal projects, contributions, Open Source" "is-primary"
  entryHID
  entrySmoothie
  entryAL
  entryPhaazonNET
  entryIRCBot
  entryMonadJournal
  entryHSFModEx
  entryLeaf
  entryPhraskell
  entrySDB

  category "School" "University, homework, fun, pizza" "is-warning"
  entryBattleRobot
  entryGameOfLifeRewrite
  entryBejeweledRewrite
  entryPongRewrite

category :: String -> Html -> AttributeValue -> Html
category name subname color = do
  section ! class_ ("section hero content is-right " <> color) $ do
    h1 ! class_ "title" ! id (fromString $ fmap toLower name) $ toHtml name
    h2 ! class_ "subtitle is-small" $ subname

entry :: Maybe AttributeValue -> Html -> Html -> Html -> Html -> Html -> Html
entry imgSrc prodName prodQuickInfo icons prodExtraInfo content = do
  section ! class_ "section" $ do
    div ! class_ "card" $ do
      div ! class_ "card-content" $ do
        div ! class_ "media" $ do
          for_ imgSrc $ \imgSrc' ->
            div ! class_ "media-left" $ do
              figure ! class_ "image is-256x256" ! style "max-width: 400px;" $
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
        void " it, because it was made in only two weeks and I tested a lot of spectra’s features in"
        void " the time of writing the production."

entrySpectra :: Html
entrySpectra =
    entry Nothing
          "spectra"
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
      p "spectra is my Rust demoscene framework. It features a lot of cool features, among:"
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
      icon "https://demozoo.org/productions/161881" "fa-microchip"
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
          "luminance"
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
          "luminance"
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

entryQuaazar :: Html
entryQuaazar =
    entry (Just "quaazar.png")
          "quaazar"
          "2015"
          icons
          extra
          content
  where
    icons = icon "https://github.com/phaazon/quaazar" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "Haskell demoscene framework"
    content = do
      p $ do
        void "quaazar was my first attempt at serious graphics programming in Haskell for demoscene"
        void " purposes. After coming to the realization that I was building something way too much"
        void " generic, I decided to discontinue the project and extract a sub part of it (luminance)."

entryIonosphere :: Html
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
        void "Ionosphere is my very first attempt at music making. Back from VIP2014, I decided to go"
        void " wild, buy Renoise 3.0 and make a tiny but lovely song in 4 hours."

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
        void "Heat Station is my second 64k intro, released at Evoke 2013. I wrote it in C++ with"
        void " skyoralis, my demoscene 3D realtime engine at that time. It was a test-release for my"
        void " engine, and I had to rush the Windows port, so take it as-is!"
      p "It ranked 4th/4, behind Farbrausch, Inque and Stroboholics."

entryLR2LR :: Html
entryLR2LR =
    entry (Just "lr2lr.png")
          "Lightning Road To Liquid Radiator"
          "Outline 2013"
          icons
          extra
          content
  where
    icons = do
      icon "http://www.youtube.com/watch?v=oUa2BvlDWYQ" "fa-youtube"
      icon "https://github.com/phaazon/lr2lr" "fa-github"
      icon "http://www.pouet.net/prod.php?which=61355" "fa-globe"
    extra = do
      p ! class_ "subtitle is-6" $ "PC Intro 64k (Linux)"
      p ! class_ "subtitle is-6" $ "Ranked 4th/9"
    content = do
      p $ do
        void "Lightning Road To Liquid Radiator is my very first release as a demoscener. It’s a"
        void " Linux 64k intro I released at Outline 2013 in the PC combined demo/intro compo"
        void " (demo, 4k, 64k); May 9th-12th. It ranked 4th/9. I’m pretty proud of it. The binary"
        void " file weighs 42k and has no data at all."
      p $ do
        void "If you plan to test it on your own Linux machine, you’ll find some hints in the README"
        void " file if you issue any trouble."

entrySkyoralis :: Html
entrySkyoralis =
    entry (Just "skyoralis.png")
          "skyoralis"
          "2013"
          icons
          extra
          content
  where
    icons = pure ()
    extra = p ! class_ "subtitle is-6" $ "C++ demoscene framework"
    content = do
      p $ do
        void "My very own demoscene engine. It’s designed to help me write my demoscene releases,"
        void " especially 64k intro. Written in C++11, OpenGL and GLSL."
      p $ do
        void "Up to now, no public version is available. The first intro using"
        void " skyoralis was Heat Station, but it actually used less than 10% of its features."
      p "I decided to close that project because I moved it into Haskell."

entryHID :: Html
entryHID =
    entry Nothing
          "hid & msi-kb-backlit"
          "2015"
          icons
          extra
          content
  where
    icons = do
      icon "https://github.com/phaazon/msi-kb-backlit" "fa-github"
      icon "http://hackage.haskell.org/package/hid" "fa-database"
    extra = p ! class_ "subtitle is-6" $ "R.E. of hardware (HID MSI laptop keyboard LEDs)"
    content = do
      p $ do
        void "Those two projects originate from my actual laptop, a MSI GS60 Ghost Pro, with backlit"
        void " keyboard. The colors are customizable and MSI has provided the users with a tool –"
        void " SteelEngine – to customize them. However, that tool doesn’t work at all under Linux"
        void " nor Mac OSX. I decided to understand how the protocol is used, and write my own tool."
        void " I also wrote the "
        a ! href "https://github.com/phaazon/hid" $ "hid"
        void " Haskell library to help me write that tool, and both the packages are available as"
        void " BSD-3 softwares."

entrySmoothie :: Html
entrySmoothie =
    entry Nothing
          "smoothie"
          "2015"
          icons
          extra
          content
  where
    icons = do
      icon "https://github.com/phaazon/smoothie" "fa-github"
      icon "http://hackage.haskell.org/package/smoothie" "fa-database"
    extra = p ! class_ "subtitle is-6" $ "N-ary spline interpolation"
    content = do
      p $ do
        void "A Haskell package to make it easy to build splines. The project has received an interest"
        void " from Haskellers. The name comes from the verb “to smooth”, which we can assimilate to"
        void " smoothed curves."

entryAL :: Html
entryAL =
    entry Nothing
          "al"
          "2014"
          icons
          extra
          content
  where
    icons = do
      icon "https://github.com/phaazon/al" "fa-github"
      icon "http://hackage.haskell.org/package/al" "fa-database"
    extra = p ! class_ "subtitle is-6" $ "OpenAL Haskell binding"
    content = do
      p "I maintain an OpenAL Haskell binding."

entryPhaazonNET :: Html
entryPhaazonNET =
    entry Nothing
          "phaazon.net"
          "2014–"
          icons
          extra
          content
  where
    icons = icon "https://github.com/phaazon/phaazon.net" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "This very website"
    content =
      p "This website is a great example of fun I have on my spare time. Written in pure Haskell."

entryIRCBot :: Html
entryIRCBot =
    entry Nothing
          "IRC bot"
          "2014–"
          icons
          extra
          content
  where
    icons = do
      icon "https://github.com/phaazon/kwak" "fa-github"
      icon "https://github.com/phaazon/tellbot" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "Rust & Haskell IRC bot"
    content = do
      p $ do
        void "A bot over IRC. It’s used to leave message to someone who’s off, to administrate a"
        void " channel, to parse and give the title behind URLs and it features a stupid yet funny"
        void " Markov chain feature that learns from people talking in a given channel."

entryMonadJournal :: Html
entryMonadJournal =
    entry Nothing
          "monad-journal"
          "2013"
          icons
          extra
          content
  where
    icons  = do
      icon "https://github.com/phaazon/monad-journal" "fa-github"
      icon "http://hackage.haskell.org/package/monad-journal" "fa-database"
    extra = p ! class_ "subtitle is-6" $ "A Haskell pure logger"
    content = do
      p "A logger solution in Haskell. It exports a typeclass and a monad transformer for ease."
      p $ do
        void "Because "
        a ! href "https://github.com/kazu-yamamoto/ghc-mod" $ "ghc-mod"
        void " uses it, monad-journal gets a lot of downloads, and I’m pretty proud of it then!"

entryHSFModEx :: Html
entryHSFModEx = entry Nothing "hsFModEx" "2013" icons extra content
  where
    icons = do
      icon "https://github.com/phaazon/hsFModEx" "fa-github"
      icon "http://hackage.haskell.org/package/FModExRaw" "fa-database"
    extra = p ! class_ "subtitle is-6" $ "Haskell FModEx binding"
    content = do
      p $ do
        void "The official Haskell FModEx binding. Up to now I maintain the raw binding. It’s intended"
        void " to be wraped up again (like OpenGLRaw). Up to now, the binding works but not all"
        void " functions are exported."
      p "2017 note: abandonned project."

entryLeaf :: Html
entryLeaf = entry Nothing "Leaf" "2013" icons extra content
  where
    icons = icon "https://github.com/phaazon/leaf" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "A Haskell portfolio generator"
    content = do
      p $ do
        void "leaf is a portfolio generator that aims to be really simple to use. My first porfolio was"
        void " written thanks to that tool! It enables you to write your portfolio in Markdown, so"
        void " it’s really convenient to use. You can find all directions to get started on Github."

entryPhraskell :: Html
entryPhraskell = entry (Just "phraskell.png") "phraskell" "2013" icons extra content
  where
    icons = icon "https://github.com/phaazon/phraskell" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "A Haskell fractal viewer!"
    content = do
      p $ do
        void "phraskell is a fractal viewer. For now, it only supports Mandelbrot set, but is"
        void " planned to support more and more fractal types. It aims to be user-friendly, and"
        void " includes these features:"
      ul $ do
        li "fractal colorschemes (not yet)"
        li "different kinds of fractal representations (standard, buddhabrot, etc.)"
        li "(not yet)"
        li "zoom with a zoom-frame in order to see what you zoomin"
        li "specify the fractal progression at runtime (not yet)"
        li "change the colors at runtime (not yet)"
        li "screenshot (not yet)"
        li "and others! (obviously not yet at all)"
      p "It’s written in Haskell, and is my first real Haskell program."
      p . b $ "01/07/13 update: I’ve saved that project for later since I’m very busy right now."

entrySDB :: Html
entrySDB = entry Nothing "sdb" "2013" icons extra content
  where
    icons = icon "https://github.com/phaazon/sdb" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "Home-made D builder"
    content = do
      p "sdb stands for Simple D Builder. It’s a D builder you can use the same way as you do with rdmd."
      p $ do
        void "Actually, you don’t want to use sdb. Why, would you say? Well, first, even if it’s"
        void " quite okay for a simple builder, rdmd is likely far away better. Furthermore, I"
        void " haven’t updated nor even read any D lines from sdb for a while. Finally, I don’t"
        void " plan to do so in the future. You can consider sdb* v0.9.5 the latest version."
        void " Forever (woah that’s so sad :( )."

entryBattleRobot :: Html
entryBattleRobot = entry (Just "battlerobot.png") "Battle Robot" "2011" icons extra content
  where
    icons = icon "https://http://dimitri.sabadie.free.fr/Download/phaazon-battlerobot_labyrinthe.tar.gz" "fa-download"
    extra = p ! class_ "subtitle is-6" $ "School Java AI game"
    content = do
      p $ do
        void "This is a Java project I had to complete the semester before the last one at"
        void " ”IUT”. Our teacher wrote a basic labyrinth and an interface for a player,"
        void " and we have to present an implementation that solves the labyrinth in local view"
        void " (the player only knows what rooms are next to him). Then all implementations"
        void " were put against each other like in a competition, and the best algorithm was"
        void " found regarding some traits like number of moves and so on."
      p $ do
        void "The first part of the project was to write a correct implementation of the"
        void " player movements. The less moves the player does to find the exit point the more"
        void " points he’s given."
      p $ do
        void "The second part of the project was to alter the implementation of our algorithm."
        void " Indeed, since the teacher had added health points to players and traps that"
        void " decrease life, we had to implement a brand new way to run the labyrinth. The"
        void " less moves and more health points left at the exit point the more points he’s"
        void " given."
      p $ do 
        void "I won both the two parts. It was meant to because all folks used random"
        void " strategies like “the right handed way” or “I have no idea where I’m going to”,"
        void " while I implemented connected and oriented graphs, with local"
        void " pseudo-deconnections (isolation of known areas of the labyrinth to make it less"
        void " dense) and path finding based on complexe heuristics. It was a really nice"
        void " project!"
      p $ do 
        void "The archive contains the java code you can execute, some labyrinths to test"
        void " (as .txt files), and two papers I have to write to explain all my choices."

entryGameOfLifeRewrite :: Html
entryGameOfLifeRewrite = entry (Just "game_of_life.png") "Rewrite of “Game Of Life”" "2011" icons extra content
  where
    icons = icon "https://github.com/phaazon/iutbx1-ds/tree/master/gol" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "C++ cellular automaton"
    content = do
      p $ do
        void "Game of Life is a cellular automaton invented by Jon Conway in 1970."
        void " At the “IUT”, we had to write our very own version, for the second semester. We"
        void " had to implement brand new features, such as being able to change the life"
        void " rules through a graphic interface."
      p $ do
        void "We also had to write that project in imperative C++, with the SFML library"
        void " and use a MV (MVC without C) software architecture."
      p $ do
        void "Note: I tried – for the fun – compiling it again. Turns out that the latest"
        void " version of SFML breaks retrocompatibility, then it’s not sure you are able"
        void " to compile it as well. Sorry then. If you really want to use it, it’s open"
        void " source, so write a patch on github, and I’ll accept the pull request! :)"

entryBejeweledRewrite :: Html
entryBejeweledRewrite = entry (Just "bejeweled.png") "Rewrite of “Bejeweled”" "2011" icons extra content
  where
    icons = icon "https://github.com/phaazon/iutbx1-ds/tree/master/bejeweled)" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "C++ game"
    content = do
      p $ do
        void "Bejeweled is a game with a 8x8 wired diamonds grid of different colors,"
        void " randomly set. Written in *imperative C++*, with a Top10 ranking system,"
        void " multiplayer, and so on…"

entryPongRewrite :: Html
entryPongRewrite = entry (Just "pong.png") "Rewrite of “Pong”" "2011" icons extra content
  where
    icons = icon "https://github.com/phaazon/iutbx1-ds/tree/master/pong)" "fa-github"
    extra = p ! class_ "subtitle is-6" $ "C++ game"
    content = do
      p $ do
        void "We don’t introduce this game anymore. Written in imperative C++. With fancy"
        void " effects. No just kidding. Written in one hour :D."
