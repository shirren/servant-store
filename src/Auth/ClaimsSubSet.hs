{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Auth.ClaimsSubSet (
  ClaimsSubSet (..)
) where

import Control.Lens ((^.))
import Data.Aeson (FromJSON, ToJSON)
import Debug.Trace (trace)
import GHC.Generics (Generic)
import Servant.Auth.Server (FromJWT (..), ToJWT)

import qualified Crypto.JWT           as Jose

data ClaimsSubSet =
  ClaimsSubSet { aud :: Maybe Jose.Audience
               , sub :: Maybe Jose.StringOrURI }
    deriving (Show, Generic)

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