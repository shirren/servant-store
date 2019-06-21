{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Products.Types (
    NewProductRequest (..)
  , Product
  , ProductT (..)
) where

import Data.Aeson (FromJSON)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table, primaryKey)

import GHC.Generics (Generic)

-- Define a type that is representative of a table with columns
-- All types that need to be stored in the database need to
-- be an instance of Beamable
data ProductT f =
  Product {
    _productId :: Columnar f Int
  , productDescription :: Columnar f Text
  , productPrice :: Columnar f Integer -- Storing the amount as cents to avoid rounding issues.
  , productPermaId :: Columnar f Text
  } deriving (Generic, Beamable)

deriving instance Show Product
deriving instance Eq Product
deriving instance Show ProductId
deriving instance Eq ProductId

{- |
Tables should have a primary key. In this case we set the column
productId as the primary key.
-}
instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ProductId . _productId

{- |
Let's define some type synonyms so we don't have to work with ProductT or Columnar directly.
Columnar is a type family defined such that for any x, Columnar Identity x = x.
This strategy is known as defunctionalization or higher-kinded data types.
-}
type Product = ProductT Identity

{- |
Our primary key for ProductT
-}
type ProductId = PrimaryKey ProductT Identity

{- |
This type is used to represent the request body for a new Product submitted
by a client to this API which would look something like;

{
 "description": "toothpaste"
 , "price" : 250
}
-}
data NewProductRequest = NewProductRequest {
  description :: Text
, price :: Integer
} deriving (Generic)

instance FromJSON NewProductRequest