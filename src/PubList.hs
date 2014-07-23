module PubList (
    PubList
  , pubImages
  , pubExecutables
  , pubVideos
  , pubArchives
  , pubAudios
  , pubTexts
  , pubPapers
  , pubUnknown
  , pubList
  ) where

import Control.Lens
import Control.Applicative
import Data.List ( isInfixOf )
import Magic

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
    return $ foldr dispatch (PubList [] [] [] [] [] [] [] []) mimed
  where
    dispatch (path,m)
        | isImage m      = pubImages %~ (:) path
        | isExecutable m = pubExecutables %~ (:) path
        | isVideo m      = pubVideos %~ (:) path
        | isArchive m    = pubArchives %~ (:) path
        | isAudio m      = pubAudios %~ (:) path
        | isText m       = pubTexts %~ (:) path
        | isPaper m      = pubPapers %~ (:) path
        | otherwise      = pubUnknown %~ (:) path
    isImage      = isInfixOf "image"
    isExecutable = isInfixOf "executable"
    isVideo      = isInfixOf "video"
    isArchive    = isInfixOf "zip"
    isAudio m    = isInfixOf "audio" m || isInfixOf "ogg" m || isInfixOf "octet-stream" m
    isText       = isInfixOf "text"
    isPaper      = isInfixOf "pdf"
