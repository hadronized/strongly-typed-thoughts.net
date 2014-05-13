module Route.Upload (
    upload
  , uploadDir
  , postFile
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans ( liftIO )
import Data.Text
import Happstack.Server as HS
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import System.FilePath ( (</>) )
import System.Directory ( copyFile )

import Wrapper ( wrapper )

uploadDir :: String
uploadDir = "assets/uploaded"

-- upload is used to upload a file, and make it available for downloads
upload :: ServerPart Response
upload =
    ok . toResponse . wrapper "Upload" $
      section ! A.id "upload-content-form" $
        H.form ! action "/postFile" ! enctype "multipart/form-data" ! A.method "POST" $ do
            input ! type_ "file" ! name "uploaded"
            input ! type_ "submit" ! value "Upload"

postFile :: ServerPart Response
postFile = do
    decodeBody (defaultBodyPolicy "/tmp" 10000000 10000000 10000000)
    (tmp,n,_) <- lookFile "uploaded"
    let filePath = uploadDir </> n
    liftIO $ do
      putStrLn $ "copying uploaded file " ++ tmp ++ " to " ++ filePath
      copyFile tmp filePath
    seeFile filePath

seeFile :: String -> ServerPart Response
seeFile filePath = do
    ok . toResponse . wrapper "Your uploaded file" $
      section ! A.id "upload-content-viewer" $
        p $ do
          toHtml ("Your file is " :: Text)
          a ! A.id "upload-content-viewer-link" ! href (toValue filePath) $
            toHtml ("here!" :: Text)
