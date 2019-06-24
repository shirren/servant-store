{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Store (
  seedData
) where

import Control.Monad

import Network.JSONApi

import qualified Orders.Data as O
import qualified Products.Data as P
import qualified Users.Data as U

-- Seed some baseline data in the application. System does not check to see if the data pre-exists
-- in the system.
seedData :: IO ()
seedData = do
  users    <- U.findAll (PageSize 10) (PageNum 0)
  products <- P.findAll (PageSize 10) (PageNum 0)
  orders   <- O.findAll (PageSize 10) (PageNum 0)
  when (null users) $ do
    _ <- U.create "john@doe.com" "John" (Just "Adrian") "Doe"
    _ <- U.create "jane@doe.com" "Jane" Nothing "Doe"
    putStrLn "Inserted users"

  when (null products) $ do
    _ <- P.create "Toothbrush" 350
    _ <- P.create "Dental floss" 120
    putStrLn "Inserted products"

  when (null orders) $ do
    reloadedUsers    <- U.findAll (PageSize 10) (PageNum 0)
    reloadedProducts <- P.findAll (PageSize 10) (PageNum 0)
    _ <- O.create (head reloadedUsers) (head reloadedProducts)
    _ <- O.create (last reloadedUsers) (last reloadedProducts)
    putStrLn "Inserted orders"

  putStrLn "Seeding complete"