{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Home (
  Home,
  home
) where

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as T (readFile)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Data.String (IsString(..))
import Servant (Get, ServantErr(..))
import Servant.HTML.Blaze (HTML)
import Servant.Server (Handler, Server, err500)
import Text.Blaze.Html (Html)
import Text.Mustache (Template, (~>), compileTemplate, object, substitute)

import Markdown (markdownToHtml)
import Wrapper (wrapper)

import Data.Text

type Home = Get '[HTML] Html

home :: Server Home
home = do
  mustacheTemplate <- liftIO $ fmap (compileTemplate "home") (T.readFile "static/content/home.md")

  case mustacheTemplate of
    Left e -> throwError $ err500 { errBody = fromString $ "home template compilation error: " ++ show e }
    Right template -> do
      age <- computeAge
      let markdown = substitute template (object ["age" ~> age])

      markdownToHtml (T.unpack markdown) >>= wrapper "Home"

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
