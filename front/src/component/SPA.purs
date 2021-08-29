-- | Top-level component of the web application.
module SPA where

import Prelude

import AboutMe (aboutMeComponent)
import Blog (blogComponent)
import Child (refresh)
import Control.Monad.RWS (get, gets, modify_, put)
import Data.Array (intersperse)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (startsWith)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import HTMLHelper (cl)
import Halogen (Component, defaultEval, mkComponent, mkEval, query)
import Halogen.HTML (HTML, a, em, footer, h1, h2, i, nav, p, slot_, span, text)
import Halogen.HTML as H
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (href, id, title)
import Router (Router, path, setPath)
import Type.Proxy (Proxy(..))

_aboutme = Proxy :: Proxy "aboutme"

_blog = Proxy :: Proxy "blog"

_browse = Proxy :: Proxy "browse"

data ActiveComponent
  = AboutMe
  | Blog
  | Browse

derive instance eqActiveComponent :: Eq ActiveComponent

instance Show ActiveComponent where
  show = case _ of
    AboutMe -> "AboutMe"
    Blog -> "Blog"
    Browse -> "Browse"

data Action
  = Init
  | SwitchComponent ActiveComponent String

data State
  = State
    { component :: ActiveComponent
    , router :: Router
    }

spaComponent :: forall query output. Int -> Component query Router output Aff
spaComponent currentYear = mkComponent { eval, initialState, render }
  where
  eval = mkEval defaultEval { handleAction = handleAction, initialize = Just Init }

  handleAction = case _ of
    Init -> do
      -- switch the component based on the location
      router <- gets $ \(State state) -> state.router
      component <- inferComponent router
      p <- path router
      liftEffect <<< log $ "inferred router is " <> show component <> " for path " <> p
      modify_ $ \(State state) -> State $ state { component = fromMaybe AboutMe component }
    SwitchComponent component url -> do
      State state <- get

      if component == state.component
      then do
        let q = case component of
                  AboutMe -> query _aboutme 0 refresh
                  Blog -> query _blog 0 refresh
                  Browse -> query _browse 0 refresh
        unit <$ q
      else do
        setPath state.router url
        put <<< State $ state { component = component }

  initialState router = State { component: AboutMe, router }

  render state = H.div [] [ navPart, renderActiveComponent state, footerPart currentYear ]

  renderActiveComponent (State state) = case state.component of
    AboutMe -> slot_ _aboutme 0 aboutMeComponent unit
    Blog -> slot_ _blog 0 blogComponent state.router
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

-- | Infer the component to use based on the current router path.
inferComponent :: forall m. MonadEffect m => Router -> m (Maybe ActiveComponent)
inferComponent = path >=> pure <<< pathToComponent
  where
  pathToComponent p
    | startsWith "/blog" p = Just Blog
    | startsWith "/browse" p = Just Browse
    | startsWith "/" p = Just AboutMe
    | otherwise = Nothing
