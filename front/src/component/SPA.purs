-- | Top-level component of the web application.
module SPA where

import Prelude
import AboutMe (aboutMeComponent)
import Data.Array (intersperse)
import Effect.Aff (Aff)
import HTMLHelper (cl)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML (HTML, a, em, footer, h1, h2, i, nav, p, slot_, span, text)
import Halogen.HTML as H
import Halogen.HTML.Properties (href, id, title)
import Type.Proxy (Proxy(..))

_aboutme = Proxy :: Proxy "aboutme"

_blog = Proxy :: Proxy "blog"

_browse = Proxy :: Proxy "browse"

data ActiveComponent
  = AboutMe
  | Blog
  | Browse

spaComponent :: forall query input output. Int -> Component query input output Aff
spaComponent currentYear = mkComponent { eval, initialState, render }
  where
  eval = mkEval defaultEval

  initialState _ = AboutMe

  render state = H.div [] [ navPart, renderActiveComponent state, footerPart currentYear ]

  renderActiveComponent = case _ of
    AboutMe -> slot_ _aboutme 0 (aboutMeComponent) unit
    Blog -> text "Slot blog"
    Browse -> text "Slot browse"

navPart :: forall w i. HTML w i
navPart =
  nav [ id "top-header", cl [ "hero", "is-medium" ] ]
    [ H.div [ cl [ "hero-body", "container", "level", "has-shadow" ] ]
        [ H.div [ cl [ "level-item", "has-text-centered" ] ]
            [ H.div [ cl [ "content" ] ]
                [ p [] [ h1 [ cl [ "title", "is-1" ] ] [ text "Dimitri Sabadie" ] ]
                , p [] [ h2 [ cl [ "subtitle", "is-4" ] ] [ em [] [ text "Do not make more tools than existing problems" ] ] ]
                ]
            ]
        , navItem "/" "phaazon.net" "fa-home"
        , navItem "https://git.phaazon.net" "git.phaazon.net" "fa-code-fork"
        , navItem "/blog" "/blog" "fa-pencil"
        , navItem "/browse" "all the memes!" "fa-cloud-download"
        ]
    ]
  where
  navItem url t icon =
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
