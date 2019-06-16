{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Orders.Types (
    NewOrderRequest (..)
  , Order
  , OrderT (..)
) where

import Data.Aeson ((.=), object, FromJSON, ToJSON, toJSON)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table, primaryKey)

import GHC.Generics (Generic)

import Products.Types (ProductT (..))

import Users.Types (UserT (..))

-- Define a type that is representative of a table with columns
-- All types that need to be stored in the database need to
-- be an instance of Beamable
data OrderT f =
  Order {
    _orderId :: Columnar f Int
  , orderUser :: PrimaryKey UserT f
  , orderProduct :: PrimaryKey ProductT f
  , orderPermaId :: Columnar f Text
  } deriving (Generic, Beamable)

deriving instance Show Order
deriving instance Eq Order
deriving instance Show OrderId
deriving instance Eq OrderId

-- Tables should have a primary key. In this case we set the column
-- orderId as the primary key.
instance Table OrderT where
  data PrimaryKey OrderT f = OrderId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = OrderId . _orderId

-- Let's define some type synonyms so we don't have to work with OrderT or Columnar directly.
-- Columnar is a type family defined such that for any x, Columnar Identity x = x.
-- This strategy is known as defunctionalization or higher-kinded data types.
type Order = OrderT Identity

-- Our primary key for OrderT
type OrderId = PrimaryKey OrderT Identity

-- We serialise our type using a representation that is more front
-- end friendly, plus we do not want to really expose the names of
-- our internal fields.
instance ToJSON Order
  where
    toJSON (Order _ _ _ permaId) =
      object ["id" .= permaId]

-- This type is used to represent the request body for a new Order submitted
-- by a client to this API which would look something like;
--
-- {
--    "productId": "universal_product_identifier"
-- }
newtype NewOrderRequest = NewOrderRequest {
  productId :: Text
} deriving (Generic)

instance FromJSON NewOrderRequest
