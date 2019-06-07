{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Products.Api (
  ProductApi,
  productsServer
) where

import           Data.List (find)
import           Data.UUID (UUID)

import qualified Products.Data as ProductsDb
import           Products.Types (Product, ProductT (..))

import           Servant ((:>), (:<|>)(..), Capture, Get, Handler, JSON, Server, err404, throwError)

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type ProductApi =
  "api" :> "v1" :> "products" :>
  (
       Get '[JSON] [Product] -- i.e. /api/v1/products
  :<|> Capture "sku" UUID :> Get '[JSON] Product -- i.e. /api/v1/products/:id
  )

-- This function "collects" all the route handler functions, which can then
-- in turn be used by the Servant server. Refer to Main.hs
productsServer :: Server ProductApi
productsServer =
  getProducts :<|>
  getProduct

-- Route handler for GET '[JSON] [Product]
getProducts :: Handler [Product]
getProducts =
  pure ProductsDb.findAll

-- Route handler for Capture "sku" Text :> Get '[JSON] Product
getProduct :: UUID -> Handler Product
getProduct pId =
  case find (\ (Product _ _ _ permaId) -> permaId == pId) ProductsDb.findAll of
    Just p -> pure p
    _      -> throwError err404