module PubList (
  ) where

import Control.Lens
import Control.Applicative
import Data.List ( isInfixOf )
import Magic
import System.FilePath ( FilePath )

-- A `PubList` is a a structure that represents public files, sorted by types.
data PubList = PubList {
    -- Images (orange).
    _pubImages      :: [FilePath]
    -- Executables (red).
  , _pubExecutables :: [FilePath]
    -- Videos (pink).
  , _pubVideos      :: [FilePath]
    -- Archives (purple).
  , _pubArchives    :: [FilePath]
    -- Audios (blue).
  , _pubAudios      :: [FilePath]
    -- Texts (green).
  , _pubTexts       :: [FilePath]
    -- Papers (white).
  , _pubPapers      :: [FilePath]
    -- Unknown (grey).
  , _pubUnknown     :: [FilePath]
  } deriving (Eq,Show)

makeLenses ''PubList
 
mimes :: [FilePath] -> IO [String]
mimes dir = do
    magicDB <- magicOpen [MagicMime,MagicPreserveAtime]
    magicLoadDefault magicDB
    mapM (magicFile magicDB) dir

pubList :: [FilePath] -> IO PubList
pubList dir = do
    mimed <- zip <$> pure dir <*> mimes dir
    return $ foldl dispatch (PubList [] [] [] [] [] [] [] []) mimed
  where
    dispatch pl (path,m)
        | isImage m      = pl & pubImages %~ (:) path
        | isExecutable m = pl & pubExecutables %~ (:) path
        | isVideo m      = pl & pubVideos %~ (:) path
        | isArchive m    = pl & pubArchives %~ (:) path
        | isAudio m      = pl & pubAudios %~ (:) path
        | isText m       = pl & pubTexts %~ (:) path
        | isPaper m      = pl & pubPapers %~ (:) path
        | otherwise      = pl & pubUnknown %~ (:) path
    isImage      = isInfixOf "image"
    isExecutable = isInfixOf "executable"
    isVideo      = isInfixOf "video"
    isArchive    = isInfixOf "zip"
    isAudio m    = isInfixOf "audio" m || isInfixOf "ogg" m
    isText       = isInfixOf "text"
    isPaper      = isInfixOf "pdf"
