-- | Blog listing, searching, reading. All the things.
module Blog where

import Prelude

import API (endpoint)
import Affjax as AX
import Affjax.ResponseFormat (json, string)
import Control.Monad.RWS (gets, modify_)
import Control.Monad.State.Class (class MonadState)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonObject, toArray, toString)
import Data.Array as A
import Data.Bifunctor (bimap, lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(Left, Right), either)
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as DT
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.String (drop, length)
import Data.String.CodeUnits (takeWhile)
import Data.String.Utils (startsWith)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log)
import Foreign.Object (Object, lookup)
import HTMLHelper (cl)
import Halogen (Component, HalogenM, defaultEval, mkComponent, mkEval)
import Halogen.HTML (PlainHTML, a, b_, blockquote_, button, em_, fromPlainHTML, h1, h2, hr_, i, p_, section, span, text)
import Halogen.HTML as H
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (href)
import Html.Renderer.Halogen as RH
import Router (Router, path, setPath)

data Action
  = Init
  | ReadArticle Slug

-- data Query

newtype Slug
  = Slug String

instance Eq Slug where
  eq (Slug a) (Slug b) = eq a b

instance Ord Slug where
  compare (Slug a) (Slug b) = compare a b

instance Show Slug where
  show (Slug s) = s

type Article
  =
    { metadata :: Metadata, content :: Maybe PlainHTML
    }

data State
  = State
    { articles :: Map Slug Article
    , currentArticle :: Maybe PlainHTML
    , router :: Router
    }

blogComponent :: forall query output m. (MonadAff m) => Component query Router output m
blogComponent = mkComponent { eval, initialState, render }
  where
  eval = mkEval defaultEval { initialize = Just Init, handleAction = handleAction }

  handleAction = case _ of
    Init -> do
      -- build the index of articles, but do not load any of them just yet
      response <- liftAff <<< map ((\res -> metadataFromJson res.body) <=< lmap AX.printError) <<< AX.get json $ endpoint "/blog"
      case response of
        Left e -> liftEffect $ log e
        Right metadata -> do
          let
            articles :: Map Slug Article
            articles =
              M.fromFoldable
                $ map
                    ( \m ->
                        Tuple m.slug
                              { metadata: m, content: Nothing
                              }
                    )
                    metadata
            currentArticle = Nothing
          modify_ $ \(State state) -> State state { articles = articles, currentArticle = currentArticle }

      -- infer whether we need to read an article
      router <- gets $ \(State state) -> state.router
      inferSlug router >>= maybe resetArticle readArticle

    ReadArticle slug -> readArticle slug

  initialState router = State { articles: M.empty, currentArticle: Nothing, router }

  render (State state) = section [ cl [ "container", "section", "content" ] ] $ case state.currentArticle of
    Just html -> [fromPlainHTML html]
    Nothing -> renderListing state

  renderListing state =
      ([ h1 [ cl [ "title" ] ]
          [ b_ [ text "Dimitri Sabadie" ], text "’s blog"
          ]
      , h2 [ cl [ "subtitle" ] ] [ em_ [ text "Functional programming, graphics, demoscene and more!" ] ]
      , hr_
      , p_
          [ text
              "This is my blog. I talk about functional programming, graphics, demoscene, optimization and many other topics!"
          ]
      , blockquote_ [ text "It is intentional that no comment can be written by readers to prevent flooding, scams and spamming." ]
      , hr_
      , p_
          [ text "Feel free to subscribe to the "
          , a [ href "/blog/feed" ]
              [ span [ cl [ "icon", "rss-feed" ] ] [ i [ cl [ "fa", "fa-rss" ] ] [] ]
              , text "feed"
              ]
          , text " to be notified when a new article is released!"
          ]
      , hr_
      ] <> renderBlogList state)

  renderBlogList state
    | M.isEmpty state.articles = [button [ cl [ "button", "is-loading"]] [text "Loading"]]
    | otherwise = map inlineBlogArticle (A.fromFoldable state.articles)

  inlineBlogArticle article = H.div [cl ["level"]] [
    span [cl ["level-left"] ] [span [cl ["level-item"]][
      a [onClick <<< const $ ReadArticle article.metadata.slug] [text article.metadata.name]
    ]],
    span [cl ["level-right"] ] [span [cl ["level-item"]][
      em_ [text $ "on " <> formatTime article.metadata.publishDate ]
    ]]
  ]

type Metadata
  = { name :: String
    , publishDate :: DateTime
    , tags :: Array String
    , slug :: Slug
    }

-- | Convert a Json value into a list of article metadata.
metadataFromJson :: Json -> Either String (Array Metadata)
metadataFromJson = caseJsonArray (Left "payload is not an array of metadata") (traverse caseMetadata)
  where
  caseMetadata :: Json -> Either String Metadata
  caseMetadata = caseJsonObject (Left "metadata is not an object") treatObject

  treatObject :: Object Json -> Either String Metadata
  treatObject o = do
    name <- extractField "missing article name" "article_name" toString
    publishDateRaw <- extractField "missing article publish date" "article_publish_date" toString
    publishDate <- DT.unformatDateTime timeFormat publishDateRaw
    tags <- extractField "missing article tags" "article_tags" $ toArray >=> traverse toString
    slug <- extractField "missing article slug" "article_slug" (map Slug <<< toString)
    pure { name, publishDate, tags, slug }
    where
    extractField :: forall a. String -> String -> (Json -> Maybe a) -> Either String a
    extractField errMsg field adapt = maybe (Left errMsg) pure (lookup field o >>= adapt)

  timeFormat = "YYYY-MM-DDTHH:mm:ssZ"

formatTime :: DateTime -> String
formatTime = DT.format fmt
  where fmt = L.fromFoldable [
     DayOfWeekNameShort, Placeholder " ", DayOfMonthTwoDigits, Placeholder " ", MonthShort, Placeholder " ",
    YearFull, Placeholder ", ", Hours24, Placeholder ":", MinutesTwoDigits, Placeholder " UTC"]

-- | Retrieve the content of a blog article.
getArticleContent :: forall m. MonadAff m => Slug -> m (Either String PlainHTML)
getArticleContent (Slug slug) =
  liftAff <<< map (bimap AX.printError (RH.render_ <<< _.body)) <<< AX.get string $ endpoint "/blog/" <> slug

-- | Reset current article to none and go back to blog article listing.
resetArticle :: forall m. MonadState State m => m Unit
resetArticle = modify_ $ \(State state) -> State state { currentArticle = Nothing }

-- | Read an article.
readArticle :: forall slots output m. MonadAff m => Slug -> HalogenM State Action slots output m Unit
readArticle slug = do
    article <- gets (\(State state) -> M.lookup slug state.articles)
    maybe cacheAndSwitch switchToArticle $ article >>= _.content
  where
    -- read and cache, then switch
    cacheAndSwitch = getArticleContent slug >>= either noSuchArticle cache

    cache html = do
      modify_ $ \(State state) -> State state
        { articles = M.update (\article -> Just article { content = Just html }) slug state.articles }
      switchToArticle html

    noSuchArticle _ = pure unit -- FIXME

    switchToArticle html = do
      modify_ $ \(State state) -> State state { currentArticle = Just html }
      router <- gets $ \(State state) -> state.router
      let Slug slug_ = slug
      setPath router $ "blog/" <> slug_

-- | Infer whether we need to read an article and return its slug.
inferSlug :: forall m. MonadEffect m => Router -> m (Maybe Slug)
inferSlug = path >=> pure <<< toSlug
  where
     toSlug p
       | startsWith "/blog/" p = Just <<< Slug <<< takeWhile (_ /= '#') $ drop (length "/blog/") p
       | otherwise = Nothing
