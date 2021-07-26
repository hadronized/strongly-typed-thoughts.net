-- Stateful side of the API.
--
-- This module stores anything related to state. Effectful operations might tap in the state.
module State
  ( APIState,
    newAPIState,
    statefulCacheFile,
    statefulUnCacheFile,
  )
where

import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Upload (MimeSortedFiles, cacheFile, mimeSort, uncacheFile, uploadDir)

data APIState = APIState
  { uploadedFiles :: TVar MimeSortedFiles
  }

newAPIState :: (MonadIO m) => m APIState
newAPIState = do
  liftIO $ do
    putStr "populating cache of uploaded files…"
    sortedFiles <- getDirectoryContents uploadDir >>= mimeSort . map (uploadDir </>) . filter (not . flip elem [".", ".."])
    putStrLn " done."

    APIState <$> newTVarIO sortedFiles

-- Stateful version of Upload.cacheFile.
statefulCacheFile :: (MonadIO m) => FilePath -> APIState -> m ()
statefulCacheFile path state = do
  let var = uploadedFiles state
  liftIO $ do
    putStrLn $ "stateful caching of " <> path
    files <- readTVarIO var >>= cacheFile path
    atomically $ writeTVar var files

-- Stateful version of Upload.uncacheFile.
statefulUnCacheFile :: (MonadIO m) => FilePath -> APIState -> m ()
statefulUnCacheFile path state = do
  let var = uploadedFiles state
  liftIO $ do
    putStrLn $ "stateful uncaching of " <> path
    files <- readTVarIO var >>= uncacheFile path
    atomically $ writeTVar var files
