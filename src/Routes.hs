module Routes where

import Control.Monad
import Happstack.Server

import Route.About
import Route.Browse
import Route.Contact
import Route.Home
import Route.Portfolio
import Route.Upload

routes :: ServerPart Response
routes = msum
    [
      dir "assets" $ serveDirectory DisableBrowsing [] "assets"
    , dir "static" $ serveDirectory DisableBrowsing [] "static"
    , dir "upload" upload
    , dir "portfolio" portfolio
    , dir "postFile" postFile
    , dir "browse" browse
    , dir "contact" contact
    , dir "about" about
    , home
    ]
