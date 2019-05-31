{-# LANGUAGE OverloadedStrings #-}

module Products.Data (
  findAll
) where

import Products.Types (Product (..))

findAll :: [Product]
findAll = [Product 1 "Toothbrush" "TB-001"
  , Product 2 "Electric Toothbrush" "ETB-001"
  , Product 3 "Dental Floss" "FL-001"]