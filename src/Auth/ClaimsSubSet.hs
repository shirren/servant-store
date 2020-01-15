{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.ClaimsSubSet (
  AuthUser (..)
, ClaimsSubSet (..)
) where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON, Result (..), fromJSON)
import Data.Text (pack)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Servant.Auth.Server (FromJWT (..), ToJWT)

import qualified Crypto.JWT           as Jose
import qualified Data.HashMap.Strict  as HM

data AuthUser =
  AuthUser { name :: String
           , email :: String }
    deriving (Generic, Show)

-- FromJSON is required as AuthUser is an instance of FromJWT
instance FromJSON AuthUser

instance FromJWT AuthUser where
  decodeJWT m =
    case HM.lookup "dat" (m ^. Jose.unregisteredClaims) of
      Nothing -> Left "Missing 'dat' claim"
      Just v  -> case fromJSON v of
        Error e -> Left $ pack e
        Success a -> trace "Decoding AuthUser JWT" Right a

-- ToJSON is required as ClaimsSubSet is an instance of ToJWT
instance ToJSON AuthUser
instance ToJWT AuthUser

data ClaimsSubSet =
  ClaimsSubSet { aud :: Maybe Jose.Audience
               , sub :: Maybe Jose.StringOrURI }
    deriving (Eq, Show, Generic)

-- FromJSON is required as ClaimsSubSet is an instance of FromJWT
instance FromJSON ClaimsSubSet

instance FromJWT ClaimsSubSet where
  decodeJWT m = do
    let aud' = m ^. Jose.claimAud
        iss = m ^. Jose.claimIss
    trace "Decoding JWT" Right (ClaimsSubSet aud' iss)

-- ToJSON is required as ClaimsSubSet is an instance of ToJWT
instance ToJSON ClaimsSubSet
instance ToJWT ClaimsSubSet