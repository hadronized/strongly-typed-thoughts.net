module Routes where

import Control.Monad
import Control.Monad.Trans ( liftIO )
import Happstack.Server
import Happstack.Server.ClientSession

import Route.About
import Route.Browse
import Route.Contact
import Route.Home
import Route.Portfolio
import Route.Upload

routes :: ServerPart Response
routes = do
    sessionConf <- liftIO (fmap mkSessionConf getDefaultKey)
    msum
      [
        dir "assets" $ serveDirectory DisableBrowsing [] "assets"
      , dir "pub" $ serveDirectory DisableBrowsing [] "pub"
      , dir "upload" upload
      , dir "saveSession" . withClientSessionT sessionConf $ saveSession
      , dir "portfolio" portfolio
      , dir "postFile" postFile
      , dir "browse" browse
      , dir "contact" contact
      , dir "about" about
      , home
      ]
