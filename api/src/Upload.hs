-- Module to manipulate uploaded files.
module Upload
  ( MimeSortedFiles (..),
    uploadDir,
    mimeSort,
    cacheFile,
    uncacheFile,
  )
where

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Default (Default (..))
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.List (isInfixOf)
import System.Process (readProcess)

-- Uploaded files, sorted by types.
data MimeSortedFiles = MimeSortedFiles
  { pubImages :: HashSet FilePath,
    pubExecutables :: HashSet FilePath,
    pubVideos :: HashSet FilePath,
    pubArchives :: HashSet FilePath,
    pubAudios :: HashSet FilePath,
    pubTexts :: HashSet FilePath,
    pubPapers :: HashSet FilePath,
    pubUnknown :: HashSet FilePath
  }
  deriving (Eq, Show)

instance Default MimeSortedFiles where
  def = MimeSortedFiles HS.empty HS.empty HS.empty HS.empty HS.empty HS.empty HS.empty HS.empty

-- Directory in which to store uploaded files.
uploadDir :: FilePath
uploadDir = "media/uploads"

-- FIXME: see #6, which is a workaround
-- Returns the mime of the input file.
readMime :: (MonadIO m) => FilePath -> m String
readMime path = liftIO $ readProcess "file" ["--mime-type", "-b", "-h", path] ""

-- Sort a list of files by mime and store them in a `MimeSortedFiles` value.
mimeSort :: (MonadIO m) => [FilePath] -> m MimeSortedFiles
mimeSort = foldM (flip cacheFile) def

-- Cache a new file in the list of mime-sorted files.
cacheFile :: (MonadIO m) => FilePath -> MimeSortedFiles -> m MimeSortedFiles
cacheFile path sortedFiles = do
  mime <- readMime path
  pure $ mimeDispatch mime sortedFiles (HS.insert path)

-- Uncache a file from the list of mime-sorted files.
uncacheFile :: (MonadIO m) => FilePath -> MimeSortedFiles -> m MimeSortedFiles
uncacheFile path sortedFiles = do
  mime <- readMime path
  pure $ mimeDispatch mime sortedFiles (HS.delete path)

-- Dispatch a function based on the mime of an input file path.
mimeDispatch :: String -> MimeSortedFiles -> (HashSet FilePath -> HashSet FilePath) -> MimeSortedFiles
mimeDispatch mime sortedFiles f
  | isImage mime = sortedFiles {pubImages = f (pubImages sortedFiles)}
  | isExecutable mime = sortedFiles {pubExecutables = f (pubExecutables sortedFiles)}
  | isVideo mime = sortedFiles {pubVideos = f (pubVideos sortedFiles)}
  | isArchive mime = sortedFiles {pubArchives = f (pubArchives sortedFiles)}
  | isAudio mime = sortedFiles {pubAudios = f (pubAudios sortedFiles)}
  | isText mime = sortedFiles {pubTexts = f (pubTexts sortedFiles)}
  | isPaper mime = sortedFiles {pubPapers = f (pubPapers sortedFiles)}
  | otherwise = sortedFiles {pubUnknown = f (pubUnknown sortedFiles)}
  where
    isImage = isInfixOf "image"
    isExecutable = isInfixOf "exec"
    isVideo = isInfixOf "video"
    isArchive = isInfixOf "zip"
    isAudio m = isInfixOf "audio" m || isInfixOf "ogg" m || isInfixOf "octet-stream" m
    isText = isInfixOf "text"
    isPaper = isInfixOf "pdf"
