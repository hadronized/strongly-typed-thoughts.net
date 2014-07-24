module Route.Browse (
    browse
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans ( liftIO )
import Data.List ( sort )
import Happstack.Server
import System.FilePath ( (</>), takeFileName )
import System.Directory ( getDirectoryContents )
import Text.Blaze.Html5 as H hiding ( map )
import Text.Blaze.Html5.Attributes as A

import PubList
import Route.Upload ( uploadDir )
import Wrapper

browse :: ServerPart Response
browse = do
    pl <- liftIO $ do
      files <- fmap (filter (not . flip elem [".",".."])) $
        getDirectoryContents uploadDir
      pubList (map (uploadDir </>) files)
    liftIO $ print pl
    ok . toResponse . wrapper "Browse files" $ do
      listView (pl^.pubImages)      "images"      "Images"
      listView (pl^.pubExecutables) "executables" "Executables"
      listView (pl^.pubVideos)      "videos"      "Videos"
      listView (pl^.pubArchives)    "archives"    "Archives"
      listView (pl^.pubAudios)      "audios"      "Audios"
      listView (pl^.pubTexts)       "texts"       "Texts"
      listView (pl^.pubPapers)      "papers"      "Papers"
      listView (pl^.pubUnknown)     "unknown"     "Unknown"
  where
    listView :: [FilePath] -> String -> String -> Html
    listView l cl tl =
        unless (null l) $
          section ! A.id (toValue $ "browse-" ++ cl) ! A.class_ "browse-content" $ do
            h3 (toHtml tl)
            ul . sequence_ . map toLink $ zip (sort l) [0..]
    toLink (filename,x) =
        li ! A.class_ (iclass x) $
          a ! href (toValue filename) $ toHtml (takeFileName filename)

iclass :: Integer -> AttributeValue
iclass x
    | even x    = "browse-content-item-even"
    | otherwise = "browse-content-item-odd"
