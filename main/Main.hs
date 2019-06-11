{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Orders.Api (OrderApi, ordersServer)

import Products.Api (ProductApi, productsServer)

import Servant ((:<|>) (..), Proxy (..), Server, serve)
import Store (seedData)

import Users.Api (UserApi, usersServer)

type API = UserApi :<|> ProductApi :<|> OrderApi

server :: Server API
server = usersServer :<|> productsServer :<|> ordersServer

app :: Application
app = serve (Proxy :: Proxy API) server

-- Start the server and expose WAI on port 3000
main :: IO ()
main = do
  seedData
  run 3000 $ logStdoutDev app
