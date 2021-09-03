-- | Browse uploaded files.
module Browse where

import Prelude
import API (endpoint)
import Affjax as AX
import Affjax.ResponseFormat (json)
import Control.Monad.State (put)
import Data.Argonaut.Core (Json, caseJsonObject, toArray, toString)
import Data.Array (fromFoldable, sortBy)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as S
import Data.String (drop, length)
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTMLHelper (cl)
import Halogen (defaultEval, mkComponent, mkEval)
import Halogen.Component (Component)
import Halogen.HTML (HTML, a, h3_, li, section, text, ul_)
import Halogen.HTML.Properties (href, id)
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

browseComponent :: forall query input output m. MonadAff m => Component query input output m
browseComponent = mkComponent { initialState, eval, render }
  where
  initialState _ =
    { pubImages: S.empty
    , pubExecutables: S.empty
    , pubVideos: S.empty
    , pubArchives: S.empty
    , pubAudios: S.empty
    , pubTexts: S.empty
    , pubPapers: S.empty
    , pubUnknown: S.empty
    }

  eval = mkEval defaultEval { handleAction = handleAction, initialize = Just Init }

  handleAction = case _ of
    Init -> do
      resp <- liftAff <<< map ((\res -> parseUploadedFiles res.body) <=< lmap AX.printError) <<< AX.get json $ endpoint "/browse"
      case resp of
        Left err -> liftEffect (log err)
        Right files -> put files

  render files =
    section [ cl [ "container", "section", "content" ] ]
      [ subSection "images" "Images" files.pubImages
      , subSection "executables" "Executables" files.pubExecutables
      , subSection "videos" "Videos" files.pubVideos
      , subSection "archives" "Archives" files.pubArchives
      , subSection "audios" "Audios" files.pubAudios
      , subSection "texts" "Texts" files.pubTexts
      , subSection "papers" "Papers" files.pubPapers
      , subSection "unknown" "Unknown" files.pubUnknown
      ]
    where
    subSection s t l =
      section [ id $ "browse-" <> s, cl [ "browse-content" ] ]
        [ h3_ [ text t ]
        , ul_ (renderedFiles l)
        ]

-- | Render a given set of filepath as a HTML <li> tag.
-- |
-- | The Bool is used for oddity.
filePathToLi :: forall w i. Boolean -> String -> HTML w i
filePathToLi isOdd path =
  li [ cl [ "browse-content-item-" <> if isOdd then "odd" else "even" ] ]
    [ a [ href path ] [ text name ]
    ]
  where
  name = dropper path

dropper :: String -> String
dropper = drop (length "media/uploads/")

renderedFiles :: forall w i. Set String -> Array (HTML w i)
renderedFiles =
  map _.html
    <<< sortBy (comparing _.path)
    <<< fromFoldable
    <<< _.lis
    <<< foldl
        ( \{ b, lis } path ->
            { b: not b, lis: Cons { html: filePathToLi b path, path: path } lis }
        )
        { b: true, lis: Nil }
