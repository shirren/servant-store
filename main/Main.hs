{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import Servant

import Users.Api (UserApi, usersServer)
import Products.Api (ProductApi, productsServer)

type API = UserApi :<|> ProductApi

server :: Server API
server = usersServer :<|> productsServer

app :: Application
app = serve (Proxy :: Proxy API) server

main :: IO ()
main =
  run 3000 app
