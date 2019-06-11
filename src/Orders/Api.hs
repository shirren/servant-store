{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- The API is the entry point into the module. Here we only export the definition
-- of our Api and the server. Note that the functions or "Handlers" are not exposed
-- thereby preserving encapsulation.
module Orders.Api (
    OrderApi
  , ordersServer
) where

import Control.Monad.IO.Class (liftIO)

import Data.DB (defaultPageSize, PageSize)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Servant

import Orders.Data (findByUser)
import Orders.Types (Order)

import Users.Data (findById)

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type OrderApi =
  "api" :> "v1" :> "users" :>
  (
    Capture "id" Text :> "orders" :> QueryParam "page[size]" PageSize :> Get '[JSON] [Maybe Order]  -- i.e. /api/v1/users/:id/orders
  )

-- Definition of our Order module API which maps our routes from the type
-- OrderApi to a collection of functions that return a type of Handler.
ordersServer :: Server OrderApi
ordersServer =
  getOrders

-- findAll returns type IO [User] which we lift to Handler [User]
getOrders :: Text -> Maybe PageSize -> Handler [Maybe Order]
getOrders uId pageSize = do
  result <- liftIO $ findById uId
  case result of
    Just user ->
      liftIO $ findByUser user $ fromMaybe defaultPageSize pageSize
    _         -> throwError err404