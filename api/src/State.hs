-- Stateful side of the API.
--
-- This module stores anything related to state. Effectful operations might tap in the state.
module State
  ( APIState,
    newAPIState,
    statefulCacheFile,
    statefulUnCacheFile,
    statefulCacheArticle,
  )
where

import Blog (ArticleMetadataStore, LiftArticleError, getCacheArticle, readMetadata, storeFromMetadata)
import Config (Config (..))
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import System.Directory (getDirectoryContents)
import System.FilePath ((</>))
import Upload (MimeSortedFiles, cacheFile, mimeSort, uncacheFile, uploadDir)

data APIState = APIState
  { -- | Uploaded files, which can be cached in and uncached.
    uploadedFiles :: TVar MimeSortedFiles,
    cachedArticles :: TVar ArticleMetadataStore
  }

newAPIState :: (MonadIO m, MonadError e m, LiftArticleError e) => Config -> m APIState
newAPIState config = do
  sortedFiles <- liftIO $ do
    putStrLn "populating uploaded files cache…"
    sortedFiles <- getDirectoryContents uploadDir >>= mimeSort . map (uploadDir </>) . filter (not . flip elem [".", ".."])
    putStrLn "  done."
    pure sortedFiles

  liftIO $ putStrLn "reading blog article index…"
  blogMetadata <- readMetadata (configBlogIndex config)
  liftIO $ putStrLn "  done."

  liftIO $ putStrLn "populating blog articles cache…"
  articleStore <- storeFromMetadata blogMetadata
  liftIO $ putStrLn "  done."

  liftIO $ APIState <$> newTVarIO sortedFiles <*> newTVarIO articleStore

-- | Stateful version of Upload.cacheFile.
statefulCacheFile :: (MonadIO m) => FilePath -> APIState -> m ()
statefulCacheFile path state = do
  let var = uploadedFiles state
  liftIO $ do
    putStrLn $ "stateful caching of " <> path
    files <- readTVarIO var >>= cacheFile path
    atomically $ writeTVar var files

-- | Stateful version of Upload.uncacheFile.
statefulUnCacheFile :: (MonadIO m) => FilePath -> APIState -> m ()
statefulUnCacheFile path state = do
  let var = uploadedFiles state
  liftIO $ do
    putStrLn $ "stateful uncaching of " <> path
    files <- readTVarIO var >>= uncacheFile path
    atomically $ writeTVar var files

-- | Stateful version of Blog.getCacheArticle that only caches.
statefulCacheArticle ::
  (MonadIO m, MonadError e m, LiftArticleError e) =>
  FilePath ->
  APIState ->
  m ()
statefulCacheArticle path state = do
  let var = cachedArticles state
  articles <- liftIO $ do
    putStrLn $ "stateful caching of article " <> path
    readTVarIO var
  (_, articles') <- getCacheArticle articles path
  liftIO . atomically $ writeTVar var articles'

-- -- | Stateful change of article.
-- statefulChangeArticle ::
--   (MonadIO m, MonadError e m, LiftArticleError e) =>
--   FilePath ->
--   APIState ->
--   m ()
-- statefulChangeArticle path state = do
--   let var = cachedArticles state
--   articles <- liftIO $ do
--     putStrLn $ "stateful change of article " <> path
--     readTVarIO var
--   case articleMetadata articles path of
--     Just metadata ->
--   pure ()

-- (_, articles') <- getCacheArticle articles path
-- liftIO . atomically $ writeTVar var articles'
