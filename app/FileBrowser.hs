{-# LANGUAGE DataKinds #-}

module FileBrowser (
    FileBrowserApi
  , fileBrowser
  , PubList
  , defaultPubList
  , refreshBrowserFiles
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, readTVarIO, writeTVar)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO(..))
import Data.List (isInfixOf, sort)
import Magic (MagicFlag(..), magicFile, magicLoadDefault, magicOpen)
import Servant (Get)
import Servant.Server (Server)
import Servant.HTML.Blaze (HTML)
import System.Directory (getDirectoryContents)
import System.FilePath ( (</>), takeFileName )
import Text.Blaze.Html5 (AttributeValue, Html, (!), a, h3, li, section, toHtml, toValue, ul)
import Text.Blaze.Html5.Attributes as A (class_, id, href)

type FileBrowserApi = Get '[HTML] Html

uploadDir :: FilePath
uploadDir = "media/uploads"

fileBrowser :: TVar PubList -> Server FileBrowserApi
fileBrowser filesTVar = liftIO . fmap renderPubList $ readTVarIO filesTVar

-- A `PubList` is a a structure that represents public files, sorted by types.
data PubList = PubList {
  -- Images (orange).
  pubImages :: [FilePath],
  -- Executables (red).
  pubExecutables :: [FilePath],
  -- Videos (pink).
  pubVideos :: [FilePath],
  -- Archives (purple).
  pubArchives :: [FilePath],
  -- Audios (blue).
  pubAudios :: [FilePath],
  -- Texts (green).
  pubTexts :: [FilePath],
  -- Papers (white).
  pubPapers :: [FilePath],
  -- Unknown (grey).
  pubUnknown :: [FilePath]
} deriving (Eq,Show)

defaultPubList :: PubList
defaultPubList = PubList [] [] [] [] [] [] [] []

-- Returns the list of mimes for each files in the input.
mimes :: [FilePath] -> IO [String]
mimes files = do
  magicDB <- magicOpen [MagicMime, MagicPreserveAtime]
  magicLoadDefault magicDB
  traverse (magicFile magicDB) files

-- Create a list of files.
createPubList :: [FilePath] -> IO PubList
createPubList files = do
    mimed <- zip <$> pure files <*> mimes files
    pure (foldr dispatch defaultPubList mimed)
  where
    dispatch (path,m) pl
      | isImage m = pl { pubImages = path : pubImages pl }
      | isExecutable m = pl { pubExecutables = path : pubExecutables pl }
      | isVideo m = pl { pubVideos = path : pubVideos pl }
      | isArchive m = pl { pubArchives = path : pubArchives pl }
      | isAudio m = pl { pubAudios = path : pubAudios pl }
      | isText m = pl { pubTexts = path : pubTexts pl }
      | isPaper m = pl { pubPapers = path : pubPapers pl }
      | otherwise = pl { pubUnknown = path : pubUnknown pl }
    isImage = isInfixOf "image"
    isExecutable = isInfixOf "exec"
    isVideo = isInfixOf "video"
    isArchive = isInfixOf "zip"
    isAudio m = isInfixOf "audio" m || isInfixOf "ogg" m || isInfixOf "octet-stream" m
    isText = isInfixOf "text"
    isPaper = isInfixOf "pdf"

-- Replace the current PubList in a TVar by freshly acquired data.
refreshBrowserFiles :: TVar PubList -> IO ()
refreshBrowserFiles var = do
  files <- fmap (filter (not . flip elem [".", ".."])) (getDirectoryContents uploadDir)
  pl <- createPubList (map (uploadDir </>) files)
  atomically (writeTVar var pl)

renderPubList :: PubList -> Html
renderPubList pl = do
    listView (pubImages pl) "images" "Images"
    listView (pubExecutables pl) "executables" "Executables"
    listView (pubVideos pl) "videos" "Videos"
    listView (pubArchives pl) "archives" "Archives"
    listView (pubAudios pl) "audios" "Audios"
    listView (pubTexts pl) "texts" "Texts"
    listView (pubPapers pl) "papers" "Papers"
    listView (pubUnknown pl) "unknown" "Unknown"
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
