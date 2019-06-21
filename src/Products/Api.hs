{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Products.Api (
  ProductApi,
  productsServer
) where

import           Control.Monad.IO.Class (liftIO)

import           Data.DB (defaultPageNum, defaultPageSize, PageNum, PageSize)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)

import qualified Network.JSONApi as JSONApi

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
    QueryParam "page[size]" PageSize :> QueryParam "page[number]" PageNum :> Get '[JSON] (JSONApi.Document PR.ProductResource) -- i.e. Http GET /api/v1/products
  :<|> Capture "perma_id" Text :> Get '[JSON] (JSONApi.Document PR.ProductResource) -- i.e. Http GET /api/v1/products/:id
  :<|> ReqBody '[JSON] NewProductRequest :> PostCreated '[JSON] (JSONApi.Document PR.ProductResource) -- i.e. HTTP POST /api/v1/products
  :<|> Capture "perma_id" Text :> ReqBody '[JSON] NewProductRequest :> Post '[JSON] (JSONApi.Document PR.ProductResource) -- i.e. HTTP POST /api/v1/products/:id
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
getProducts :: Maybe PageSize -> Maybe PageNum -> Handler (JSONApi.Document PR.ProductResource)
getProducts pageSize pageNum = do
  products <- liftIO $ findAll (fromMaybe defaultPageSize pageSize) (fromMaybe defaultPageNum pageNum)
  productCount <- liftIO countProducts
  let productResources = mkProductResource <$> products
  pure $ JSONApi.mkDocument productResources
         (Just $ JSONApi.indexLinks (head productResources) pageSize pageNum productCount)
         (Just $ JSONApi.mkMeta $ JSONApi.Pagination pageSize pageNum productCount)

{- |
Route handler for Capture "perma_id" Text :> Get '[JSON] Product
-}
getProduct :: Text -> Handler (JSONApi.Document PR.ProductResource)
getProduct pId = do
  mProduct <- liftIO $ findById pId
  case mProduct of
    Just p ->
      pure $ JSONApi.mkDocument [mkProductResource p] Nothing Nothing
    _      ->
      throwError err404

{- |
Add a new product and serialise back the persisted product which now includes a universal
identifier for the product. Note that if the request does not conform to the shape of the
NewProductRequest Servant generates a 400 bad request. We do not need to handle this
error scenario.
-}
createProduct :: NewProductRequest -> Handler (JSONApi.Document PR.ProductResource)
createProduct newProduct = do
  p <- liftIO $ create (description newProduct) (price newProduct)
  pure $ JSONApi.mkDocument [mkProductResource p] Nothing Nothing

  {- |
This endpoint allows a client to update product state. Note how we use NewProductRequest for the
update as well. This is because we expose the same properties for update as we do for a create.
Refer to the user update where we only allow a subset of properties in an update.
-}
updateProduct :: Text -> NewProductRequest -> Handler (JSONApi.Document PR.ProductResource)
updateProduct pId productData = do
  mProduct <- liftIO $ findById pId
  case mProduct of
    Just p -> do
      updatedP <- liftIO $ updateState p { productDescription = description productData, productPrice = price productData }
      pure $ JSONApi.mkDocument [mkProductResource updatedP] Nothing Nothing
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