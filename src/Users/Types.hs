-- {-# LANGUAGE DeriveGeneric #-}
-- {-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Users.Types (
    User (..)
  , UserT (..)
) where

import Data.Aeson ((.=), object, ToJSON, toJSON)
import Data.Text (Text)
import Database.Beam (Columnar, Identity, PrimaryKey)

import GHC.Generics (Generic)

-- Define a type that is representative of a table with columns
data UserT f =
  User {
    _userEmailAddress :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userLastName :: Columnar f Text
  , _userPermaId :: Columnar f Text
  }

-- Let's define some type synonyms so we don't have to work with UserT or Columnar directly.
-- Columnar is a type family defined such that for any x, Columnar Identity x = x.
-- This strategy is known as defunctionalization or higher-kinded data types.
type User = UserT Identity

-- Our primary key for UserT
type UserId = PrimaryKey UserT Identity

instance ToJSON User
  where
    toJSON (User e fname lname _) =
      object ["emailAddress" .= e
            , "firstName" .= fname
            , "lastName" .= lname
            ]
