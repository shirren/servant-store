{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Products.Resources (
    ProductResource (..)
  , toResource
  ) where

import           Data.Aeson.TH
import           Data.Text (Text)

import           Network.JSONApi (ResourcefulEntity (..))
import qualified Network.JSONApi as JSONApi

data ProductResource = ProductResource
  { resourceId :: Text
  , price :: Integer
  , description :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''ProductResource)

instance ResourcefulEntity ProductResource where
  resourceIdentifier = resourceId
  resourceType _ = "products"
  resourceLinks = Just . JSONApi.showLink
  resourceMetaData _ = Nothing
  resourceRelationships _ = Nothing
