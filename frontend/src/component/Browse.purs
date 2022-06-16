-- | Browse uploaded files.
module Browse where

import Prelude
import API (endpoint)
import Affjax as AX
import Affjax.ResponseFormat (json)
import Control.Monad.State (modify_, put)
import Data.Argonaut.Core (Json, caseJsonObject, toArray, toString)
import Data.Array (catMaybes, filter, fromFoldable, null, sortBy)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Data.String (toLower)
import Data.String.Utils (includes)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTMLHelper (cl)
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen.Component (Component)
import Halogen.HTML (HTML, a, h3_, i, input, li, section, span, text, ul_)
import Halogen.HTML as H
import Halogen.HTML.Events (onValueInput)
import Halogen.HTML.Properties (href, id, placeholder)
import JSON (extractField)

type UploadedFiles
  = { images :: Set String
    , applications :: Set String
    , videos :: Set String
    , audios :: Set String
    , texts :: Set String
    , papers :: Set String
    , unknown :: Set String
    }

type State
  = { files :: UploadedFiles
    , fileFilter :: String
    }

parseUploadedFiles :: Json -> Either String UploadedFiles
parseUploadedFiles = caseJsonObject (Left "not an object") treatObject
  where
  treatObject o = do
    images <- extractField o "missing images" "images" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    applications <- extractField o "missing applications" "applications" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    videos <- extractField o "missing videos" "videos" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    audios <- extractField o "missing audios" "audios" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    texts <- extractField o "missing texts" "texts" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    papers <- extractField o "missing papers" "papers" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    unknown <- extractField o "missing unknowns" "unknowns" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    pure $ { images, applications, videos, audios, texts, papers, unknown }

data Action
  = Init
  | FilterFiles String

browseComponent :: forall query input output m. MonadAff m => Component query input output m
browseComponent = mkComponent { initialState, eval, render }
  where
  initialState _ =
    { files:
        { images: S.empty
        , applications: S.empty
        , videos: S.empty
        , audios: S.empty
        , texts: S.empty
        , papers: S.empty
        , unknown: S.empty
        }
    , fileFilter: ""
    }

  eval = mkEval defaultEval { handleAction = handleAction, initialize = Just Init }

  handleAction = case _ of
    Init -> do
      resp <- liftAff <<< map ((\res -> parseUploadedFiles res.body) <=< lmap AX.printError) <<< AX.get json $ endpoint "/browse"
      case resp of
        Left err -> liftEffect (log err)
        Right files -> put { files, fileFilter: "" }
    FilterFiles f -> modify_ $ _ { fileFilter = f }

  render state =
    section [ cl [ "container", "section", "content" ] ] <<< catMaybes
      $ [ Just searchInput
        , subSection state.fileFilter "images" "Images" state.files.images
        , subSection state.fileFilter "applications" "Applications" state.files.applications
        , subSection state.fileFilter "videos" "Videos" state.files.videos
        , subSection state.fileFilter "audios" "Audios" state.files.audios
        , subSection state.fileFilter "texts" "Texts" state.files.texts
        , subSection state.fileFilter "papers" "Papers" state.files.papers
        , subSection state.fileFilter "unknown" "Unknown" state.files.unknown
        ]
    where
    searchInput =
      H.div [ cl [ "control", "browse-input", "has-icons-left" ] ]
        [ input [ cl [ "input" ], placeholder "Search uploaded files…", onValueInput FilterFiles ]
        , span [ cl [ "icon", "is-left", "is-small" ] ] [ i [ cl [ "fa", "fa-search" ] ] [] ]
        ]

    subSection fileFilter s t l =
      let
        rendered = renderedFiles fileFilter l
      in
        if null rendered then
          Nothing
        else
          Just
            $ section [ id $ "browse-" <> s, cl [ "browse-content" ] ]
                [ h3_ [ text t ]
                , ul_ (renderedFiles fileFilter l)
                ]

-- | Render a given set of filepath as a HTML <li> tag.
-- |
-- | The Bool is used for oddity.
filePathToLi :: forall w i. Boolean -> String -> { html :: HTML w i, name :: String }
filePathToLi isOdd path =
  let
    html =
      li [ cl [ "browse-content-item-" <> if isOdd then "odd" else "even" ] ]
        [ a [ href $ "media/uploads/" <> path ] [ text path ]
        ]
  in
    { html, name: path }

renderedFiles :: forall w i. String -> Set String -> Array (HTML w i)
renderedFiles fileFilter =
  map _.html
    <<< sortBy (comparing _.name)
    <<< filter (\s -> includes fileFilter' (toLower s.name))
    <<< fromFoldable
    <<< _.lis
    <<< foldl
        ( \{ b, lis } path ->
            { b: not b, lis: Cons (filePathToLi b path) lis }
        )
        { b: true, lis: Nil }
  where
  fileFilter' = toLower fileFilter
