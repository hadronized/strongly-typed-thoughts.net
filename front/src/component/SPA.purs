-- | Top-level component of the web application.
module SPA where

import Prelude
import AboutMe (aboutMeComponent)
import Blog (blogComponent)
import Control.Monad.RWS (get, put)
import Data.Array (intersperse)
import Effect.Aff (Aff)
import HTMLHelper (cl)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML (HTML, a, em, footer, h1, h2, i, nav, p, slot_, span, text)
import Halogen.HTML as H
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (href, id, title)
import Router (Router, setPath)
import Type.Proxy (Proxy(..))

_aboutme = Proxy :: Proxy "aboutme"

_blog = Proxy :: Proxy "blog"

_browse = Proxy :: Proxy "browse"

data ActiveComponent
  = AboutMe
  | Blog
  | Browse

data Action
  = SwitchComponent ActiveComponent String

data State
  = State
    { component :: ActiveComponent
    , router :: Router
    }

spaComponent :: forall query output. Int -> Component query Router output Aff
spaComponent currentYear = mkComponent { eval, initialState, render }
  where
  eval = mkEval defaultEval { handleAction = handleAction }

  handleAction = case _ of
    SwitchComponent component url -> do
      State state <- get
      setPath state.router url
      put <<< State $ state { component = component }

  initialState router = State { component: AboutMe, router }

  render state = H.div [] [ navPart, renderActiveComponent state, footerPart currentYear ]

  renderActiveComponent (State state) = case state.component of
    AboutMe -> slot_ _aboutme 0 aboutMeComponent unit
    Blog -> slot_ _blog 0 blogComponent unit
    Browse -> text "Slot browse"

navPart :: forall w. HTML w Action
navPart =
  nav [ id "top-header", cl [ "hero", "is-medium" ] ]
    [ H.div [ cl [ "hero-body", "container", "level", "has-shadow" ] ]
        [ H.div [ cl [ "level-item", "has-text-centered" ] ]
            [ H.div [ cl [ "content" ] ]
                [ p [] [ h1 [ cl [ "title", "is-1" ] ] [ text "Dimitri Sabadie" ] ]
                , p [] [ h2 [ cl [ "subtitle", "is-4" ] ] [ em [] [ text "Do not make more tools than existing problems" ] ] ]
                ]
            ]
        , navItem "phaazon.net" "fa-home" (SwitchComponent AboutMe "/")
        , navItemExternal "git.phaazon.net" "fa-code-fork" "https://git.phaazon.net"
        , navItem "/blog" "fa-pencil" (SwitchComponent Blog "/blog")
        , navItem "all the memes!" "fa-cloud-download" (SwitchComponent Browse "/browse")
        ]
    ]
  where
  navItem t icon action =
    a [ title t, cl [ "level-item" ], onClick (const action) ]
      [ span [ cl [ "icon", "is-large" ] ]
          [ i [ cl [ "fa", icon ] ] []
          ]
      ]

  navItemExternal t icon url =
    a [ href url, title t, cl [ "level-item" ] ]
      [ span [ cl [ "icon", "is-large" ] ]
          [ i [ cl [ "fa", icon ] ] []
          ]
      ]

footerPart :: forall w i. Int -> HTML w i
footerPart year = footer [ cl [ "footer" ] ] [ H.div [ cl [ "content", "has-text-centered" ] ] content ]
  where
  content =
    [ p [] <<< intersperse (text " ")
        $ [ iconItem "https://github.com/phaazon" "fa-github"
          , iconItem "https://twitter.com/phaazon_" "fa-twitter"
          , iconItem "https://www.linkedin.com/in/dimitri-sabadie-97a9009b/" "fa-linkedin-square"
          , iconItem "https://soundcloud.com/phaazon" "fa-soundcloud"
          , iconItem "https://stackoverflow.com/users/1784267/phaazon" "fa-stack-overflow"
          , iconItem "/media/uploads/cv.pdf" "fa-graduation-cap"
          ]
    , p [] <<< intersperse (text " · ")
        $ [ badgeLink "https://haskell.org" "Haskell"
          , badgeLink "http://haskell-servant.readthedocs.io" "servant"
          , badgeLink "https://github.com/purescript-halogen/purescript-halogen" "Halogen"
          , badgeLink "https://www.purescript.org" "PureScript"
          , badgeLink "http://bulma.io" "bulma"
          ]
    , p [] $ map text [ "Copyright © 2014—", show year, ", Dimitri Sabadie" ]
    ]

  iconItem url iconName = a [ cl [ "icon" ], href url ] [ i [ cl [ "fa", iconName ] ] [] ]

  badgeLink url name = a [ href url ] [ text name ]