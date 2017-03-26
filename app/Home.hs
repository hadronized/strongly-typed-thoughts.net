{-# LANGUAGE DataKinds #-}

module Home (
  API,
  home
) where

import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (readFile)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Data.String (IsString(..))
import Servant (Get, ServantErr(..), throwError)
import Servant.HTML.Blaze (HTML)
import Servant.Server (Server, err500)
import Text.Blaze.Html (Html)
import Text.Mustache ((~>), compileTemplate, object, substitute)

import Markdown (markdownToHtml)
import Wrapper (wrapper)

type API = Get '[HTML] Html

home :: Server API
home = do
  mustacheTemplate <- liftIO $ fmap (compileTemplate "home") (T.readFile "static/content/home.md")

  case mustacheTemplate of
    Left e -> throwError $ err500 { errBody = fromString $ "home template compilation error: " ++ show e }
    Right template -> do
      age <- computeAge
      let markdown = substitute template (object ["age" ~> age])
      let content = markdownToHtml (T.unpack markdown)
      pure (wrapper "Home" content)

computeAge :: (MonadIO m) => m Int
computeAge = liftIO $ fmap (age . toGregorian . utctDay) getCurrentTime
  where
    (byear, bmonth, bday) = birthdate
    age :: (Integer, Int, Int) -> Int
    age (year, month, day)
      | month > bmonth || (month == bmonth && day > bday) = fromIntegral year - byear
      | otherwise = fromIntegral year - byear - 1

birthdate :: (Int, Int, Int)
birthdate = (1992, 3, 15)
