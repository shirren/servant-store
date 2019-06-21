{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Data.DB (
    defaultPageNum
  , defaultPageSize
  , getConnection
  , PageNum
  , PageSize
  , SortBy (..)
  , SortDirection (..)
  , storeDb
  , StoreDb (..)
) where

import Data.Text (Text)
import Database.Beam (Database, DatabaseSettings, TableEntity, defaultDbSettings)
import Database.Beam.Postgres (Postgres)
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Database.PostgreSQL.Simple.Internal (Connection)

import GHC.Generics (Generic)

import Orders.Types (OrderT (..))
import Products.Types (ProductT (..))
import Users.Types (UserT (..))

-- We can specify limits on the number of rows we would like back from the database
type PageSize = Int
type PageNum = Int

defaultPageSize :: PageSize
defaultPageSize = 10

defaultPageNum :: PageNum
defaultPageNum = 0

data SortDirection = ASC | DESC

data SortBy = SortBy {
    fieldName :: Text
  , direction :: SortDirection
}

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
  connectPostgreSQL "dbname=store_dev host=localhost user=? password=? port=5432"
