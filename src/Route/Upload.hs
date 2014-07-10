module Route.Upload (
    upload
  , uploadDir
  , postFile
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans ( liftIO )
import Data.Text
import Happstack.Server
import Happstack.Server.ClientSession
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import System.FilePath ( (</>) )
import System.Directory ( copyFile )

import Session
import Wrapper ( wrapper )

uploadDir :: String
uploadDir = "assets/uploaded"

-- Upload is used to upload a file, and make it available for downloads.
--
-- It uses a session to handle login / password.
upload :: ServerPart Response
upload = do
    -- get the session config
    sessionConf <- liftIO (fmap mkSessionConf getDefaultKey)
    withClientSessionT sessionConf $ do
      session <- getSession
      case session^.sessionCred of
        Nothing   -> forbidden . toResponse $
          wrapper "You don’t have credentials to do that, sorry." (return ())
        Just cred -> do
          ok . toResponse . wrapper ("Upload – " ++ show (cred^.credLogin)) $
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
      section ! A.id "upload-content-viewer" $ do
        "Your file is "
        a ! A.id "upload-content-viewer-link" ! href (toValue filePath) $
          "here!"
