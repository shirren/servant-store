{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

import qualified Network.JSONApi as JSONApi

import           Servant

import           Orders.Data (countOrders, create, findByUser)
import qualified Orders.Resources as OR
import           Orders.Types (NewOrderRequest (..), OrderT (..), Order)

import qualified Orders.Data as O
import qualified Products.Data as P
import qualified Users.Data as U

{- |
Example on how to define a nested route. The detailed route follows
a very common convention of nesting resources under /api/v1.

For a very good blog post on how to set these up please refer to
https://qfpl.io/posts/nested-routes-in-servant/
-}
type OrderApi =
  "api" :> "v1" :> "users" :>
  (
    Capture "id" Text :> "orders" :> QueryParam "page[size]" PageSize :> QueryParam "page[number]" PageNum :> Get '[JSON] (JSONApi.Document OR.OrderResource) -- i.e. Http GET /api/v1/users/:id/orders
    :<|> Capture "id" Text :> "orders" :> ReqBody '[JSON] NewOrderRequest :> PostCreated '[JSON] (JSONApi.Document OR.OrderResource)
    :<|> Capture "id" Text :> "orders" :> Capture "orderId" Text :> Delete '[JSON] NoContent
  )

{- |
Definition of our Order module API which maps our routes from the type
OrderApi to a collection of functions that return a type of Handler.
-}
ordersServer :: Server OrderApi
ordersServer =
  getOrders :<|>
  createOrder :<|>
  cancelOrder

{- |
findAll returns type IO [User] which we lift to Handler [User]
-}
getOrders :: Text -> Maybe PageSize -> Maybe PageNum -> Handler (JSONApi.Document OR.OrderResource)
getOrders uId pageSize pageNum = do
  -- We call the User.findById function which returns a IO (Maybe User). So
  -- we need to call liftIO which has the signature IO a -> m a.
  mUser <- liftIO $ U.findById uId
  case mUser of
    Just user -> do
      orders <- liftIO $ findByUser user (fromMaybe defaultPageSize pageSize) (fromMaybe defaultPageNum pageNum)
      orderCount <- liftIO countOrders
      let orderResources = mkOrderResource <$> orders
      pure $ JSONApi.mkDocument orderResources
           (Just $ JSONApi.indexLinks (head orderResources) pageSize pageNum orderCount)
           (Just $ JSONApi.mkMeta $ JSONApi.Pagination pageSize pageNum orderCount)
    _         -> throwError err404

{- |
This endpoint is used when a user makes a new order for a particular product. If either the
user or the product does not exist we return a 404, else we return the newly created order.
Note that if the request does not conform to the shape of the
NewOrderRequest Servant generates a 400 bad request. We do not need to handle this
error scenario.
-}
createOrder :: Text -> NewOrderRequest -> Handler (JSONApi.Document OR.OrderResource)
createOrder uId newProductRequest = do
  -- We call the User.findById function which returns a IO (Maybe User). So
  -- we need to call liftIO which has the signature IO a -> m a.
  mUser <- liftIO $ U.findById uId
  mProd <- liftIO $ P.findById (productId newProductRequest)
  if isJust mUser && isJust mProd
  then do
    o <- liftIO $ create (fromJust mUser) (fromJust mProd)
    pure $ JSONApi.mkDocument [mkOrderResource $ Just o] Nothing Nothing
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

{- |
Helper function that maps from OrderT to OrderResource
-}
mkOrderResource :: Maybe Order -> OR.OrderResource
mkOrderResource (Just order) = OR.OrderResource {
    OR.resourceId = orderPermaId order
  }
mkOrderResource Nothing = OR.OrderResource {
  OR.resourceId = "Uknown"
}
