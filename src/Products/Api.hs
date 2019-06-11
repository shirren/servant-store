{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Products.Api (
  ProductApi,
  productsServer
) where

import Control.Monad.IO.Class (liftIO)

import Data.DB (defaultPageSize, PageSize)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Products.Data (findAll, findById)
import Products.Types (Product)

import Servant ((:>), (:<|>)(..), Capture, Get, Handler, JSON, Server, err404, throwError, QueryParam)

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type ProductApi =
  "api" :> "v1" :> "products" :>
  (
    QueryParam "page[size]" PageSize :> Get '[JSON] [Product] -- i.e. /api/v1/products
  :<|> Capture "perma_id" Text :> Get '[JSON] Product -- i.e. /api/v1/products/:id
  )

-- This function "collects" all the route handler functions, which can then
-- in turn be used by the Servant server. Refer to Main.hs
productsServer :: Server ProductApi
productsServer =
  getProducts :<|>
  getProduct

-- Route handler for GET '[JSON] [Product]
-- findAll returns type IO [Product] which we lift to Handler [Product]
getProducts :: Maybe PageSize -> Handler [Product]
getProducts pageSize =
  liftIO $ findAll $ fromMaybe defaultPageSize pageSize

-- Route handler for Capture "perma_id" Text :> Get '[JSON] Product
getProduct :: Text -> Handler Product
getProduct pId = do
  result <- liftIO $ findById pId
  case result of
    Just p -> pure p
    _      -> throwError err404