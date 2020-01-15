{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Auth.ClaimsSubSet (AuthUser (..))
import Control.Concurrent (forkIO)
import Crypto.JOSE.JWA.JWS (Alg (HS256))
import Crypto.JWT (
    ClaimsSet
  , JWK
  , JWKSet
  , JWTError)
import Data.Aeson (eitherDecode')
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Orders.Api (OrderApi, ordersServer)
import Products.Api (ProductApi, productsServer)
import Servant (
    (:<|>) (..)
  , (:>)
  , Proxy (..)
  , Context ((:.), EmptyContext)
  , Server
  , serveWithContext)
import Servant.Auth.Server (
    Auth
  , JWT
  , JWTSettings (..)
  , defaultJWTSettings
  , defaultCookieSettings
  , generateKey
  , jwtAlg
  , makeJWT)
import Store (seedData)
import Users.Api (UserApi, usersServer)

import qualified Network.HTTP.Simple as HTTP

type API auths =
       (Auth auths AuthUser :> UserApi)
  :<|> ProductApi
  :<|> OrderApi

server :: Server (API auths)
server = usersServer :<|>
         productsServer :<|>
         ordersServer

-- Start the server and expose WAI on port 3000
main :: IO ()
main = do
  seedData
  key <- generateKey
  let jwtCfg = defaultJWTSettings key
      jwtCfg' = jwtCfg { jwtAlg = Just HS256 }
      cfg = defaultCookieSettings :. jwtCfg' :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])
      req = HTTP.parseRequest_ "https://paidright-ci.au.auth0.com/.well-known/jwks.json"
  response <- HTTP.httpLBS req
  let body = HTTP.getResponseBody response
  case eitherDecode' body :: Either String JWKSet of
    Left error ->
      print error
    Right json ->
      print json
  -- run 3000 $ logStdoutDev (serveWithContext api cfg server)
