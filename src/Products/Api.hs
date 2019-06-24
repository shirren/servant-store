{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Products.Api (
  ProductApi,
  productsServer
) where

import           Common.Paging
import           Control.Monad.IO.Class (liftIO)

import           Data.Text (Text)

import           Network.JSONApi

import           Products.Data (countProducts, create, findAll, findById, updateState)
import qualified Products.Resources as PR
import           Products.Types (NewProductRequest (..), Product, ProductT (..))

import           Servant ((:>), (:<|>)(..), Capture, Get, Handler, JSON, Post, PostCreated, ReqBody, Server, err404, throwError, QueryParam)

{- |
Example on how to define a nested route. The detailed route follows
a very common convention of nesting resources under /api/v1.

For a very good blog post on how to set these up please refer to
https://qfpl.io/posts/nested-routes-in-servant/
-}
type ProductApi =
  "api" :> "v1" :> "products" :>
  (
    QueryParam "page[size]" Int :> QueryParam "page[number]" Int :> Get '[JSON] (Document PR.ProductResource) -- i.e. Http GET /api/v1/products
  :<|> Capture "perma_id" Text :> Get '[JSON] (Document PR.ProductResource) -- i.e. Http GET /api/v1/products/:id
  :<|> ReqBody '[JSON] NewProductRequest :> PostCreated '[JSON] (Document PR.ProductResource) -- i.e. HTTP POST /api/v1/products
  :<|> Capture "perma_id" Text :> ReqBody '[JSON] NewProductRequest :> Post '[JSON] (Document PR.ProductResource) -- i.e. HTTP POST /api/v1/products/:id
  )

{- |
This function "collects" all the route handler functions, which can then
in turn be used by the Servant server. Refer to Main.hs
-}
productsServer :: Server ProductApi
productsServer =
  getProducts :<|>
  getProduct :<|>
  createProduct :<|>
  updateProduct

{- |
Route handler for GET '[JSON] [Product]
findAll returns type IO [Product] which we lift to Handler [Product]
-}
getProducts :: Maybe Int -> Maybe Int -> Handler (Document PR.ProductResource)
getProducts pageSize pageNum = do
  products <- liftIO $ findAll (wrapPgSize pageSize) (wrapPgNum pageNum)
  productCount <- liftIO countProducts
  let productResources = mkProductResource <$> products
  let pagination = Pagination (wrapPgSize pageSize) (wrapPgNum pageNum) (wrapResCount productCount)
  pure $ mkDocument productResources (Just $ indexLinks "/products" pagination) (Just $ mkMeta pagination)

{- |
Route handler for Capture "perma_id" Text :> Get '[JSON] Product
-}
getProduct :: Text -> Handler (Document PR.ProductResource)
getProduct pId = do
  mProduct <- liftIO $ findById pId
  case mProduct of
    Just p ->
      pure $ mkSimpleDocument [mkProductResource p]
    _      ->
      throwError err404

{- |
Add a new product and serialise back the persisted product which now includes a universal
identifier for the product. Note that if the request does not conform to the shape of the
NewProductRequest Servant generates a 400 bad request. We do not need to handle this
error scenario.
-}
createProduct :: NewProductRequest -> Handler (Document PR.ProductResource)
createProduct newProduct = do
  p <- liftIO $ create (description newProduct) (price newProduct)
  pure $ mkSimpleDocument [mkProductResource p]

  {- |
This endpoint allows a client to update product state. Note how we use NewProductRequest for the
update as well. This is because we expose the same properties for update as we do for a create.
Refer to the user update where we only allow a subset of properties in an update.
-}
updateProduct :: Text -> NewProductRequest -> Handler (Document PR.ProductResource)
updateProduct pId productData = do
  mProduct <- liftIO $ findById pId
  case mProduct of
    Just p -> do
      updatedP <- liftIO $ updateState p { productDescription = description productData, productPrice = price productData }
      pure $ mkSimpleDocument [mkProductResource updatedP]
    _      ->
      throwError err404

{- |
Helper function that maps from ProductT to ProductResource
-}
mkProductResource :: Product -> PR.ProductResource
mkProductResource prod = PR.ProductResource {
    PR.resourceId = productPermaId prod
  , PR.description = productDescription prod
  , PR.price = productPrice prod
  }