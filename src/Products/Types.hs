{-# LANGUAGE DeriveGeneric #-}

module Products.Types (
  Product (..)
) where

import Data.Aeson (ToJSON)
import Data.Text (Text)

import GHC.Generics (Generic)

data Product = Product {
  productId  :: Int
  , description :: Text
  , sku :: Text
} deriving (Generic)

instance ToJSON Product
