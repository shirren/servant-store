{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)

import Products.Api (ProductApi, productsServer)

import Servant ((:<|>) (..), Proxy (..), Server, serve)
import Store (seedData)

import Users.Api (UserApi, usersServer)

type API = UserApi :<|> ProductApi

server :: Server API
server = usersServer :<|> productsServer

app :: Application
app = serve (Proxy :: Proxy API) server

-- Start the server and expose WAI on port 3000
main :: IO ()
main = do
  seedData
  run 3000 app
