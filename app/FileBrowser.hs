{-# LANGUAGE DataKinds #-}

module FileBrowser (
  FileBrowser,
  fileBrowser
) where

import Servant (Get)
import Servant.Server (Server)
import Servant.HTML.Blaze (HTML)
import System.Directory (getDirectoryContents)
import Text.Blaze.Html5 (Html)

type FileBrowser = Get '[HTML] Html;

fileBrowser :: Server FileBrowser
fileBrowser = do
  liftIO $ do
    files <- fmap (filter (not . flip elem [".", ".."])) (getDirectoryContents "media/uploads")

  pure ha

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

-- Returns the list of mimes for each files in the input.
mimes :: [FilePath] -> IO [String]
mimes files = do
  magicDB <- magicOpen [MagicMime,MagicPreserveAtime]
  magicLoadDefault magicDB
  traverse (magicFile magicDB) files
