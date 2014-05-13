module Routes where

import Control.Monad
import Happstack.Server

import Route.About
import Route.Contact
import Route.Browse
import Route.Home
import Route.Upload

routes :: ServerPart Response
routes = msum
    [
      dir "assets" $ serveDirectory DisableBrowsing [] "assets"
    , dir "upload" upload
    , dir "postFile" postFile
    , dir "browse" browse
    , dir "contact" contact
    , dir "about" about
    , home
    ]