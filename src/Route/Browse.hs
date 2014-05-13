module Route.Browse (
    browse
  ) where

import Control.Monad.Trans ( liftIO )
import Data.List ( sort )
import Happstack.Server
import System.FilePath ( (</>) )
import System.Directory ( getDirectoryContents )
import Text.Blaze.Html5 as H hiding ( map )
import Text.Blaze.Html5.Attributes as A

import Route.Upload ( uploadDir )
import Wrapper

browse :: ServerPart Response
browse = do
    files <- liftIO . fmap (sort . filter (not . flip elem [".",".."])) $
      getDirectoryContents uploadDir
    ok . toResponse . wrapper "Browse files" $
      section ! A.id "browse-content" $
        ul . sequence_ . map toLink $ zip files [0..]
  where
    toLink (filename,x) =
        li ! A.class_ (iclass x) $
          a ! href (toValue $ uploadDir </> filename) $ toHtml filename

iclass :: Integer -> AttributeValue
iclass x
    | even x    = "browse-content-item-even"
    | otherwise = "browse-content-item-odd"