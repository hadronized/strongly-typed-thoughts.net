{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Webapp (
  webapp
) where

import Servant (Proxy(..), (:>), (:<|>)(..))
import Servant.Server (Application, Server, serve)

import Home
import Portfolio

webapp :: Application
webapp = serve (Proxy :: Proxy API) server

type API =
       Home
  :<|> "portfolio" :> Portfolio

server :: Server API
server = home :<|> portfolio
