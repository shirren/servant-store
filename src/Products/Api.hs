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

type ProductApi =
  "products" :> Get '[JSON] [Product] :<|>
  "products" :> Capture "sku" Text :> Get '[JSON] Product

productsServer :: Server ProductApi
productsServer =
  getProducts :<|>
  getProduct

getProducts :: Handler [Product]
getProducts =
  pure ProductsDb.findAll

getProduct :: Text -> Handler Product
getProduct productSku =
  case find (\ p -> sku p == productSku) ProductsDb.findAll of
    Just p -> pure p
    _      -> throwError err404