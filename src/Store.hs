{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Store (
    seedData
  , storeDb
) where

import Database.Beam (Database
  , DatabaseSettings
  , TableEntity
  , defaultDbSettings
  , default_
  , just_
  , insert
  , nothing_
  , val_
  , insertExpressions
  , pk)
import Database.Beam.Postgres (Postgres, runBeamPostgresDebug)
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.PostgreSQL.Simple (connectPostgreSQL)

import GHC.Generics (Generic)

import Orders.Types (OrderT (..))
import Products.Types (ProductT (..))
import Users.Types (UserT (..))

-- A database is defined as a collection on entities or tables.
data StoreDb f = StoreDb {
    _storeUsers :: f (TableEntity UserT)
  , _storeProducts :: f (TableEntity ProductT)
  , _storeOrders :: f (TableEntity OrderT)
  } deriving (Generic, Database Postgres)

storeDb :: DatabaseSettings be StoreDb
storeDb = defaultDbSettings

-- Seed some baseline data in the application. System does not check to see if the data pre-exists
-- in the system.
seedData :: IO ()
seedData = do
  conn <- connectPostgreSQL "dbname=store_dev host=localhost user=postgres password=root port=5432"
  (user1, user2, product1, product2, order1) <- runBeamPostgresDebug putStrLn {- for debug output -} conn $ do
    [user1, user2] <- runInsertReturningList $ insert (_storeUsers storeDb) $
      insertExpressions [ User default_ "james@example.com" "James" nothing_ "Smith" default_
                        , User default_ "john@example.com" "John" (just_ "Adrian") "Doe" default_]

    [product1, product2] <- runInsertReturningList $ insert (_storeProducts storeDb) $
      insertExpressions [ Product default_ "Toothbrush" 350 default_
                        , Product default_ "Dental floss" 120 default_ ]

    [order1] <- runInsertReturningList $ insert (_storeOrders storeDb) $
      insertExpressions [ Order default_ (val_ $ pk user1) (val_ $ pk product1) default_ ]

    pure (user1, user2, product1, product2, order1)

  putStrLn ("Seeding complete..." <> show user1 <> show user2 <> show product1 <> show product2 <> show order1)
