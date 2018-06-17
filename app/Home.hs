{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Home (
  HomeApi,
  home
) where

import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import qualified Data.Text.IO as T
import Data.Time (getCurrentTime, toGregorian, utctDay)
import Data.String (IsString(..))
import Servant (Get, ServantErr(..))
import Servant.HTML.Blaze (HTML)
import Servant.Server (Server, err500)
import Prelude hiding (div, id)
import Text.Blaze.Html5 ((!), Html, div, script, toHtml)
import Text.Blaze.Html5.Attributes (class_, id)
import Text.Mustache ((~>), compileTemplate, object, substitute)

import Markdown (markdownToHtml)
import Wrapper (wrapper)

type HomeApi = Get '[HTML] Html

home :: Server HomeApi
home = do
  mustacheTemplate <- liftIO $ fmap (compileTemplate "home") (T.readFile "static/content/home.md")

  case mustacheTemplate of
    Left e -> throwError $ err500 { errBody = fromString $ "home template compilation error: " ++ show e }
    Right template -> do
      age <- computeAge
      let markdown = substitute template (object ["age" ~> age])

      homeContent <- markdownToHtml markdown

      wrapper "Home" $ do
        div ! class_ "columns" $ do
          div ! id "home" ! class_ "column content" $ homeContent

          div ! class_ "column" $ do
            div ! id "feed" $ pure ()
            script . toHtml . unlines $
              [ "GitHubActivity.feed({"
              , "  username: \"phaazon\","
              , "  selector: \"#feed\","
              , "  limit: 10"
              , "});"
              ]

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
