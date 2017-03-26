{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Webapp (
  webapp
) where

import Servant (Proxy(..), (:>), (:<|>)(..))
import Servant.Server (Application, Server, serve)

import FileBrowser (FileBrowser, fileBrowser)
import Home (Home, home)
import Portfolio (Portfolio, portfolio)

webapp :: Application
webapp = serve (Proxy :: Proxy API) server

type API =
       Home
  :<|> "portfolio" :> Portfolio
  :<|> "browse" :> FileBrowser

server :: Server API
server = home :<|> portfolio :<|> fileBrowser
