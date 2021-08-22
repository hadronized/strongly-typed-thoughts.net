-- | Blog listing, searching, reading. All the things.
module Blog where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat (json)
import Data.Argonaut.Core (Json, caseJsonArray, caseJsonObject, toArray, toString)
import Data.DateTime (DateTime)
import Data.Either (Either(Left, Right))
import Data.Formatter.DateTime (unformatDateTime)
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Foreign.Object (Object, lookup)
import HTMLHelper (cl)
import Halogen (Component, defaultEval, mkComponent, mkEval)
import Halogen.HTML (a, b_, blockquote_, em_, h1, h2, hr_, i, p_, section, span, text)
import Halogen.HTML.Properties (href)

data Action
  = Init

blogComponent :: forall query input output m. (MonadAff m) => Component query input output m
blogComponent = mkComponent { eval, initialState, render }
  where
  eval = mkEval defaultEval { initialize = Just Init, handleAction = handleAction }

  handleAction = case _ of
    Init -> do
      response <- liftAff (AX.get json "/api/v1/blog")
      liftEffect
        $ case response of
            Left e -> log $ AX.printError e
            Right r -> log $ show r.status

  initialState _ = unit

  render _ =
    section [ cl [ "container", "section", "content" ] ]
      [ h1 [ cl [ "title" ] ]
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
      ]

data Metadata
  = Metadata
    { articleName :: String
    , articlePath :: String
    , articlePublishDate :: DateTime
    , articleTags :: Array String
    , articleSlug :: String
    }

-- | Convert a Json value into a list of article metadata.
metadataFromJson :: Json -> Either String (Array Metadata)
metadataFromJson = caseJsonArray (Left "payload is not an array of metadata") (traverse caseMetadata)
  where
  caseMetadata :: Json -> Either String Metadata
  caseMetadata = caseJsonObject (Left "metadata is not an object") treatObject

  treatObject :: Object Json -> Either String Metadata
  treatObject o = do
    articleName <- extractField "missing article name" "articleName" toString
    articlePath <- extractField "missing article path" "articlePath" toString
    articlePublishDateRaw <- extractField "missing article publish date" "articlePublishDate" toString
    articlePublishDate <- unformatDateTime "" articlePublishDateRaw
    articleTags <- extractField "missing article tags" "articleTags" $ toArray >=> traverse toString
    articleSlug <- extractField "missing article tags" "articleTags" toString
    pure $ Metadata { articleName, articlePath, articlePublishDate, articleTags, articleSlug }
    where
    extractField :: forall a. String -> String -> (Json -> Maybe a) -> Either String a
    extractField errMsg field adapt = maybe (Left errMsg) pure (lookup field o >>= adapt)
