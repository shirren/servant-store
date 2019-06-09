{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Orders.Types (
    Order
  , OrderT (..)
) where

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
  , _user :: PrimaryKey UserT f
  , _product :: PrimaryKey ProductT f
  , _orderPermaId :: Columnar f Text
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
