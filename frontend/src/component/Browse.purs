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
import Data.String (drop, length, toLower)
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
  = { pubImages :: Set String
    , pubExecutables :: Set String
    , pubVideos :: Set String
    , pubArchives :: Set String
    , pubAudios :: Set String
    , pubTexts :: Set String
    , pubPapers :: Set String
    , pubUnknown :: Set String
    }

type State
  = { files :: UploadedFiles
    , fileFilter :: String
    }

parseUploadedFiles :: Json -> Either String UploadedFiles
parseUploadedFiles = caseJsonObject (Left "not an object") treatObject
  where
  treatObject o = do
    pubImages <- extractField o "missing images" "pub_images" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    pubExecutables <- extractField o "missing executables" "pub_executables" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    pubVideos <- extractField o "missing videos" "pub_videos" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    pubArchives <- extractField o "missing archives" "pub_archives" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    pubAudios <- extractField o "missing audios" "pub_audios" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    pubTexts <- extractField o "missing texts" "pub_texts" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    pubPapers <- extractField o "missing papers" "pub_papers" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    pubUnknown <- extractField o "missing unknowns" "pub_unknown" <<< map (map S.fromFoldable) $ toArray >=> traverse toString
    pure $ { pubImages, pubExecutables, pubVideos, pubArchives, pubAudios, pubTexts, pubPapers, pubUnknown }

data Action
  = Init
  | FilterFiles String

browseComponent :: forall query input output m. MonadAff m => Component query input output m
browseComponent = mkComponent { initialState, eval, render }
  where
  initialState _ =
    { files:
        { pubImages: S.empty
        , pubExecutables: S.empty
        , pubVideos: S.empty
        , pubArchives: S.empty
        , pubAudios: S.empty
        , pubTexts: S.empty
        , pubPapers: S.empty
        , pubUnknown: S.empty
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
        , subSection state.fileFilter "images" "Images" state.files.pubImages
        , subSection state.fileFilter "executables" "Executables" state.files.pubExecutables
        , subSection state.fileFilter "videos" "Videos" state.files.pubVideos
        , subSection state.fileFilter "archives" "Archives" state.files.pubArchives
        , subSection state.fileFilter "audios" "Audios" state.files.pubAudios
        , subSection state.fileFilter "texts" "Texts" state.files.pubTexts
        , subSection state.fileFilter "papers" "Papers" state.files.pubPapers
        , subSection state.fileFilter "unknown" "Unknown" state.files.pubUnknown
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
        [ a [ href path ] [ text name ]
        ]
  in
    { html, name }
  where
  name = dropper path

dropper :: String -> String
dropper = drop (length "media/uploads/")

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
