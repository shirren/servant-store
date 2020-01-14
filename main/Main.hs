{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Auth.ClaimsSubSet (ClaimsSubSet (..))
-- import Control.Concurrent (forkIO)
-- import Control.Monad (forever)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Orders.Api (OrderApi, ordersServer)
import Products.Api (ProductApi, productsServer)
import Servant (
    (:<|>) (..)
  , (:>)
  , Proxy (..)
  , Context ((:.), EmptyContext)
  , Server, serveWithContext)
import Servant.Auth.Server (
    Auth
  , JWT
  , defaultJWTSettings
  , defaultCookieSettings
  , generateKey)
import Store (seedData)
import Users.Api (UserApi, usersServer)

type API auths =
       (Auth auths ClaimsSubSet :> UserApi)
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
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      api = Proxy :: Proxy (API '[JWT])
  run 3000 $ logStdoutDev (serveWithContext api cfg server)

  -- putStrLn "Started server on localhost:3000"
  -- putStrLn "Enter name and email separated by a space for a new token"

  -- forever $ do
  --    xs <- words <$> getLine
  --    case xs of
  --      [iss', aud'] -> do
  --        etoken <- makeJWT (ClaimsSubSet iss' aud') jwtCfg Nothing
  --        case etoken of
  --          Left e -> putStrLn $ "Error generating token:t" ++ show e
  --          Right v -> putStrLn $ "New token:\t" ++ show v
  --      _ -> putStrLn "Expecting a name and email separated by spaces"
