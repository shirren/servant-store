{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- The API is the entry point into the module. Here we only export the definition
-- of our Api and the server. Note that the functions or "Handlers" are not exposed
-- thereby preserving encapsulation.
module Orders.Api (
    OrderApi
  , ordersServer
) where

import           Control.Monad.IO.Class (liftIO)

import           Data.DB (defaultPageNum, defaultPageSize, PageNum, PageSize)
import           Data.Maybe (fromJust, fromMaybe, isJust)
import           Data.Text (Text)

import           Servant

import           Orders.Data (create, findByUser)
import           Orders.Types (NewOrderRequest (..), Order)

import qualified Orders.Data as O
import qualified Products.Data as P
import qualified Users.Data as U

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type OrderApi =
  "api" :> "v1" :> "users" :>
  (
    Capture "id" Text :> "orders" :> QueryParam "page[size]" PageSize :> QueryParam "page[number]" PageNum :> Get '[JSON] [Maybe Order]  -- i.e. Http GET /api/v1/users/:id/orders
    :<|> Capture "id" Text :> "orders" :> ReqBody '[JSON] NewOrderRequest :> PostCreated '[JSON] Order
    :<|> Capture "id" Text :> "orders" :> Capture "orderId" Text :> Delete '[JSON] NoContent
  )

-- Definition of our Order module API which maps our routes from the type
-- OrderApi to a collection of functions that return a type of Handler.
ordersServer :: Server OrderApi
ordersServer =
  getOrders :<|>
  createOrder :<|>
  cancelOrder

-- findAll returns type IO [User] which we lift to Handler [User]
getOrders :: Text -> Maybe PageSize -> Maybe PageNum -> Handler [Maybe Order]
getOrders uId pageSize pageNum = do
  -- We call the User.findById function which returns a IO (Maybe User). So
  -- we need to call liftIO which has the signature IO a -> m a.
  mUser <- liftIO $ U.findById uId
  case mUser of
    Just user ->
      liftIO $ findByUser user (fromMaybe defaultPageSize pageSize) (fromMaybe defaultPageNum pageNum)
    _         -> throwError err404

-- This endpoint is used when a user makes a new order for a particular product. If either the
-- user or the product does not exist we return a 404, else we return the newly created order.
-- Note that if the request does not conform to the shape of the
-- NewOrderRequest Servant generates a 400 bad request. We do not need to handle this
-- error scenario.
createOrder :: Text -> NewOrderRequest -> Handler Order
createOrder uId newProductRequest = do
  -- We call the User.findById function which returns a IO (Maybe User). So
  -- we need to call liftIO which has the signature IO a -> m a.
  mUser <- liftIO $ U.findById uId
  mProd <- liftIO $ P.findById (productId newProductRequest)
  if isJust mUser && isJust mProd
  then liftIO $ create (fromJust mUser) (fromJust mProd)
  else throwError err404

-- In this function we cancel an order by deleting the order from the database. If either the user
-- or the order cannot be found we return the traditional 404 status code.
cancelOrder :: Text -> Text -> Handler NoContent
cancelOrder uId oId = do
  mUser  <- liftIO $ U.findById uId
  mOrder <- liftIO $ O.findById oId
  if isJust mUser && isJust mOrder
  then do
    _ <- liftIO $ O.remove $ fromJust mOrder
    return NoContent
  else throwError err404