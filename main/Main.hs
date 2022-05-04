{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Auth.ClaimsSubSet (AuthUser (..), ClaimsSubSet (..))
import Control.Concurrent (forkIO)
import Control.Lens (over)
import Crypto.JOSE.JWA.JWS (Alg (RS256))
import Crypto.JOSE.JWK (JWKSet (..))
import Crypto.JWT (
    ClaimsSet
  , JWK
  , JWKSet
  , JWTError)
import Data.Aeson (eitherDecode, parseJSON, FromJSON, Value)
import Data.HexString (hexString, toBytes)
import Data.Aeson.Lens (_String, key, values)
import Data.Char (toLower)
import Data.Text (Text)
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

import qualified Data.ByteString.Base64.URL as ByteString.Base64
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Simple as HTTP

type API auths =
       (Auth auths ClaimsSubSet :> UserApi)
  :<|> ProductApi
  :<|> OrderApi

server :: Server (API auths)
server = usersServer :<|>
         productsServer :<|>
         ordersServer

newtype JWKSetX5THex = JWKSetX5THex {
  toJWKSet :: JWKSet
} deriving Show

instance FromJSON JWKSetX5THex where
  parseJSON = fmap JWKSetX5THex . parseJSON . fixXt5Encoding

fixXt5Encoding :: Value -> Value
fixXt5Encoding =
  let thumbprints = key "keys" . values . key "x5t" . _String
  in  over thumbprints hexToBytes

hexToBytes :: Text -> Text
hexToBytes =
  let decode    = ByteString.Base64.decodeLenient . Text.encodeUtf8
      transform = toBytes . hexString . ByteString.Char8.map toLower
      encode    = Text.decodeUtf8 . ByteString.Base64.encode
  in  encode . transform . decode

-- Start the server and expose WAI on port 3000
main :: IO ()
main = do
  seedData
  let req = HTTP.parseRequest_ "https://<change_me>.auth0.com/.well-known/jwks.json"
  response <- HTTP.httpLBS req
  let body = HTTP.getResponseBody response
  case eitherDecode body :: Either String JWKSetX5THex of
    Left error ->
      print $ "Error: " <> show error
    Right (JWKSetX5THex (JWKSet set)) -> do
      let jwtCfg = defaultJWTSettings (head set)
          cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
          api = Proxy :: Proxy (API '[JWT])
      run 3000 $ logStdoutDev (serveWithContext api cfg server)
