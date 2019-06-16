{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Products.Api (
  ProductApi,
  productsServer
) where

import Control.Monad.IO.Class (liftIO)

import Data.DB (defaultPageNum, defaultPageSize, PageNum, PageSize)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Products.Data (create, findAll, findById)
import Products.Types (NewProductRequest (..), Product)

import Servant ((:>), (:<|>)(..), Capture, Get, Handler, JSON, Post, ReqBody, Server, err404, throwError, QueryParam)

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type ProductApi =
  "api" :> "v1" :> "products" :>
  (
    QueryParam "page[size]" PageSize :> QueryParam "page[number]" PageNum :> Get '[JSON] [Product] -- i.e. Http GET /api/v1/products
  :<|> Capture "perma_id" Text :> Get '[JSON] Product -- i.e. Http GET /api/v1/products/:id
  :<|> ReqBody '[JSON] NewProductRequest :> Post '[JSON] Product -- i.e. HTTP POST /api/v1/products
  )

-- This function "collects" all the route handler functions, which can then
-- in turn be used by the Servant server. Refer to Main.hs
productsServer :: Server ProductApi
productsServer =
  getProducts :<|>
  getProduct :<|>
  createProduct

-- Route handler for GET '[JSON] [Product]
-- findAll returns type IO [Product] which we lift to Handler [Product]
getProducts :: Maybe PageSize -> Maybe PageNum -> Handler [Product]
getProducts pageSize pageNum =
  liftIO $ findAll (fromMaybe defaultPageSize pageSize) (fromMaybe defaultPageNum pageNum)

-- Route handler for Capture "perma_id" Text :> Get '[JSON] Product
getProduct :: Text -> Handler Product
getProduct pId = do
  result <- liftIO $ findById pId
  case result of
    Just p -> pure p
    _      -> throwError err404

-- Add a new product and serialise back the persisted product which now includes a universal
-- identifier for the product. Note that if the request does not conform to the shape of the
-- NewProductRequest Servant generates a 400 bad request. We do not need to handle this
-- error scenario.
createProduct :: NewProductRequest -> Handler Product
createProduct newProduct =
  liftIO $ create (description newProduct) (price newProduct)