{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Products.Api (
  ProductApi,
  productsServer
) where

import           Data.List (find)
import           Data.Text (Text)

import           Servant

import qualified Products.Data as ProductsDb
import           Products.Types (Product (..))

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type ProductApi =
  "api" :> "v1" :> "products" :>
  (
       Get '[JSON] [Product] -- i.e. /api/v1/products
  :<|> Capture "sku" Text :> Get '[JSON] Product -- i.e. /api/v1/products/:sku
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
getProduct :: Text -> Handler Product
getProduct productSku =
  case find (\ p -> sku p == productSku) ProductsDb.findAll of
    Just p -> pure p
    _      -> throwError err404