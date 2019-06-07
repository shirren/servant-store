{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Products.Types (
    Product
  , ProductT (..)
) where

import Data.Aeson ((.=), object, ToJSON, toJSON)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Table, primaryKey)
import Database.Beam.Postgres (PgMoney)

import GHC.Generics (Generic)

-- Define a type that is representative of a table with columns
-- All types that need to be stored in the database need to
-- be an instance of Beamable
data ProductT f =
  Product {
    _productId :: Columnar f Int
  , _description :: Columnar f Text
  , _price :: Columnar f PgMoney
  , _productPermaId :: Columnar f Text
  } deriving (Generic, Beamable)

deriving instance Show Product
deriving instance Eq Product
deriving instance Show ProductId
deriving instance Eq ProductId

-- Tables should have a primary key. In this case we set the column
-- productId as the primary key.
instance Table ProductT where
  data PrimaryKey ProductT f = ProductId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = ProductId . _productId

-- Let's define some type synonyms so we don't have to work with ProductT or Columnar directly.
-- Columnar is a type family defined such that for any x, Columnar Identity x = x.
-- This strategy is known as defunctionalization or higher-kinded data types.
type Product = ProductT Identity

-- Our primary key for ProductT
type ProductId = PrimaryKey ProductT Identity

-- We serialise our type using a representation that is more front
-- end friendly, plus we do not want to really expose the names of
-- our internal fields.
instance ToJSON Product
  where
    toJSON (Product _ d p permaId) =
      object ["description" .= d
            , "price" .= show p
            , "id" .= permaId
            ]
