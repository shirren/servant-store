{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Orders.Resources (
    OrderResource (..)
  , toResource
  ) where

import           Data.Aeson.TH
import           Data.Text (Text)

import           Network.JSONApi (ResourcefulEntity (..))
import qualified Network.JSONApi as JSONApi

newtype OrderResource = OrderResource
  { resourceId :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''OrderResource)

instance ResourcefulEntity OrderResource where
  resourceIdentifier = resourceId
  resourceType _ = "orders"
  resourceLinks = Just . JSONApi.showLink
  resourceMetaData _ = Nothing
  resourceRelationships _ = Nothing
