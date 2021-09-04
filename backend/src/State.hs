{-# LANGUAGE OverloadedStrings #-}

-- Stateful side of the API.
--
-- This module stores anything related to state. Effectful operations might tap in the state.
module State
  ( APIState,
    newAPIState,
    cachedIndexHtml,
    getUploadedFiles,
    statefulCacheFile,
    statefulUnCacheFile,
    listBlogArticleMetadata,
    getBlogArticleContent,
    cachedGPGKeyFile,
  )
where

import Blog (ArticleMetadata, ArticleMetadataStore, LiftArticleError, Slug, getArticleContent, metadataArticles, readMetadata, storeFromMetadata)
import Config (Config (..))
import Control.Concurrent.STM (atomically, readTVarIO, writeTVar)
import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import qualified Data.Text.IO as T
import System.Directory (doesFileExist, getDirectoryContents)
import System.FilePath ((</>))
import Text.Blaze.Html (Html, preEscapedToHtml)
import Upload (MimeSortedFiles, cacheFile, mimeSort, uncacheFile, uploadDir)

data APIState = APIState
  { -- | index.html; used a lot by the front-end so better cache it
    indexHtml :: Html,
    -- | Uploaded files, which can be cached in and uncached.
    uploadedFiles :: TVar MimeSortedFiles,
    -- | Cached articles.
    cachedArticles :: TVar ArticleMetadataStore,
    -- | Cached GPG file.
    cachedGPGKeyFile :: Text
  }

newAPIState :: (MonadIO m, MonadError e m, LiftArticleError e) => Config -> m APIState
newAPIState config = do
  index <- liftIO $ do
    putStrLn "caching index.html"
    fmap preEscapedToHtml . T.readFile $ configFrontDir config </> "index.html"

  sortedFiles <- liftIO $ do
    putStrLn "populating uploaded files cache…"
    sortedFiles <- getDirectoryContents uploadDir >>= mimeSort . map (uploadDir </>) . filter (not . flip elem [".", ".."])
    putStrLn "done."
    pure sortedFiles

  liftIO $ putStrLn "reading blog article index…"
  blogMetadata <- readMetadata (configBlogIndex config)
  liftIO $ putStrLn "done."

  liftIO $ putStrLn "populating blog articles cache…"
  articleStore <- storeFromMetadata blogMetadata
  liftIO $ putStrLn "done."

  liftIO $ putStrLn "reading GPG key file"
  let gpgPath = configGPGKeyFile config
  gpgKeyFile <-
    liftIO $
      doesFileExist gpgPath >>= \exists ->
        if exists
          then T.readFile (configGPGKeyFile config)
          else do
            putStrLn "warning: no GPG key file; will not be able to serve it correctly"
            pure ""

  liftIO $ APIState <$> pure index <*> newTVarIO sortedFiles <*> newTVarIO articleStore <*> pure gpgKeyFile

-- | Get the cached index.html.
cachedIndexHtml :: APIState -> Html
cachedIndexHtml = indexHtml

-- | Mon cul c’est du téflon.
getUploadedFiles :: (MonadIO m) => APIState -> m MimeSortedFiles
getUploadedFiles = liftIO . readTVarIO . uploadedFiles

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

listBlogArticleMetadata :: (MonadIO m) => APIState -> m [ArticleMetadata]
listBlogArticleMetadata state =
  liftIO . fmap metadataArticles . readTVarIO $ cachedArticles state

getBlogArticleContent :: (MonadIO m, MonadError e m, LiftArticleError e) => APIState -> Slug -> m Html
getBlogArticleContent state slug =
  liftIO (readTVarIO $ cachedArticles state) >>= flip getArticleContent slug