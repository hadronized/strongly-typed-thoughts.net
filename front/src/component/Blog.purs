-- | Blog listing, searching, reading. All the things.
module Blog where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat (json)
import Control.Monad.RWS (put)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonObject, toArray, toString)
import Data.Array as A
import Data.Bifunctor (lmap)
import Data.DateTime (DateTime)
import Data.Either (Either(Left, Right))
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as DT
import Data.List as L
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign.Object (Object, lookup)
import HTMLHelper (cl)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML (a, b_, blockquote_, button, em_, h1, h2, hr_, i, p_, section, span, text)
import Halogen.HTML as H
import Halogen.HTML.Properties (href)

data Action
  = Init

newtype Slug
  = Slug String

instance Eq Slug where
  eq (Slug a) (Slug b) = eq a b

instance Ord Slug where
  compare (Slug a) (Slug b) = compare a b

type Article
  =
    { metadata :: Metadata, content :: Maybe String
    }

type State
  =
    { articles :: Map Slug Article
    }

blogComponent :: forall query input output m. (MonadAff m) => Component query input output m
blogComponent = mkComponent { eval, initialState, render }
  where
  eval = mkEval defaultEval { initialize = Just Init, handleAction = handleAction }

  handleAction = case _ of
    Init -> do
      response <- liftAff <<< map ((\res -> metadataFromJson res.body) <=< lmap AX.printError) $ AX.get json "/api/v1/blog"
      case response of
        Left e -> liftEffect $ log e
        Right metadata -> do
          liftEffect <<< log $ show metadata
          let
            articles :: Map Slug Article
            articles =
              M.fromFoldable
                $ map
                    ( \m ->
                        Tuple (Slug m.slug)
                              { metadata: m, content: Nothing
                              }
                    )
                    metadata
          put $ { articles }

  initialState _ = { articles: M.empty }

  render state =
    section [ cl [ "container", "section", "content" ] ]
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
      a [href $ "/blog/" <> article.metadata.slug] [text article.metadata.name]
    ]],
    span [cl ["level-right"] ] [span [cl ["level-item"]][
      em_ [text $ "on " <> formatTime article.metadata.publishDate ]
    ]]
  ]

type Metadata
  = { name :: String
    , publishDate :: DateTime
    , tags :: Array String
    , slug :: String
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
    slug <- extractField "missing article slug" "article_slug" toString
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
