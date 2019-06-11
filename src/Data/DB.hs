{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Data.DB (
    StoreDb (..)
  , getConnection
  , storeDb
) where

import Database.Beam (Database, DatabaseSettings, TableEntity, defaultDbSettings)
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Database.PostgreSQL.Simple.Internal (Connection)

import GHC.Generics (Generic)

import Orders.Types (OrderT (..))
import Products.Types (ProductT (..))
import Users.Types (UserT (..))

-- A database is defined as a collection on entities or tables.
data StoreDb f = StoreDb {
    storeUsers :: f (TableEntity UserT)
  , storeProducts :: f (TableEntity ProductT)
  , storeOrders :: f (TableEntity OrderT)
  } deriving (Generic, Database Postgres)

storeDb :: DatabaseSettings be StoreDb
storeDb = defaultDbSettings

getConnection :: IO Connection
getConnection =
  connectPostgreSQL "dbname=store_dev host=localhost user=postgres password=root port=5432"