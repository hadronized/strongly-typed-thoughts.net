{-# LANGUAGE DataKinds #-}

module Webapp (
  webapp
) where

import Servant (Proxy(..))
import Servant.Server (Application, Server, serve)

import qualified Home

type API = Home.API

webapp :: Application
webapp = serve (Proxy :: Proxy API) server

server :: Server API
server = Home.home

