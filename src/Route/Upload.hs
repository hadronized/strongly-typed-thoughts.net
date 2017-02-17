{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Route.Upload (
    upload
  , uploadDir
  , postFile
  , saveSession
  ) where

import Control.Lens
import Control.Monad.Trans ( liftIO )
import Data.ByteString ( ByteString )
import Data.Char ( isAlpha )
import Data.String ( IsString(..) )
import Data.Text ( Text )
import Database.SQLite.Simple ( Query, query, withConnection )
import qualified Data.Text as T
import Happstack.Server
import Happstack.Server.ClientSession
import Text.Blaze.Html5 as H hiding ( head, map )
import Text.Blaze.Html5.Attributes as A
import System.FilePath ( (</>) )
import System.Directory ( copyFile )

import Session
import Password
import Wrapper ( wrapper )

uploadDir :: String
uploadDir = "pub"

-- Upload is used to upload a file, and make it available for downloads.
--
-- It uses a session to handle login / password.
upload :: ServerPart Response
upload = do
    -- get the session config
    sessionConf <- liftIO (fmap mkSessionConf getDefaultKey)
    withClientSessionT sessionConf $ do
      sess <- getSession
      case sess^.sessionCred of
        Nothing   -> loginForm "Please enter your credentials"
        Just cred -> login cred

postFile :: ServerPart Response
postFile = do
    decodeBody (defaultBodyPolicy "/tmp" 20000000 20000000 20000000)
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
        _ <- "Your file is "
        a ! A.id "upload-content-viewer-link" ! href (toValue filePath) $
          "here!"

loginForm :: T.Text -> ClientSessionT Session (ServerPartT IO) Response
loginForm msg =
    ok . toResponse . wrapper "Login" $
      section ! A.id "upload-login-form" $ do
        p (toHtml msg)
        H.form ! action "/saveSession" ! enctype "multipart/form-data" ! A.method "POST" $ do
          input ! type_ "label" ! name "login"
          input ! type_ "password" ! name "password"
          input ! type_ "submit" ! value "Login"

saveSession :: ClientSessionT Session (ServerPartT IO) Response
saveSession = do
    decodeBody (defaultBodyPolicy "/tmp" 20000000 20000000 20000000)
    [loginInfo,pwdInfo] <- mapM look ["login","password"]
    let sess = session loginInfo pwdInfo
    putSession sess
    login $ Cred loginInfo (hashPwd pwdInfo)

login :: Cred -> ClientSessionT Session (ServerPartT IO) Response
login (Cred loginInfo pwdInfo) = do
    pwd <- liftIO $ do
      withConnection "db/local.db" $ \conn -> do
        --  FIXME: weird issue with parameters here
        pwd :: [[ByteString]] <- query conn selectQuery selectParams
        pure pwd
    if length pwd == 1 then do
      let [dbPwd] = head pwd

      if pwdInfo == dbPwd then do
        ok . toResponse . wrapper ("Upload – " ++ loginInfo) $
          section ! A.id "upload-content-form" $
            H.form ! action "/postFile" ! enctype "multipart/form-data" ! A.method "POST" $ do
             input ! type_ "file" ! name "uploaded"
             input ! type_ "submit" ! value "Upload"
        else
          loginForm "Wrong login and/or password :("
    else do
      loginForm "Wrong login and/or password!"
  where
    selectQuery :: Query
    selectQuery = "select cred_pwd from Credentials where cred_name = ?"
    selectParams :: [Text]
    selectParams = [fromString $ sanitize loginInfo]
    sanitize = filter isAlpha
