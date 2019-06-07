{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Users.Types (
    User
  , UserT (..)
) where

import Data.Aeson ((.=), object, ToJSON, toJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Nullable, Table, primaryKey)

import GHC.Generics (Generic)

-- Define a type that is representative of a table with columns
-- All types that need to be stored in the database need to
-- be an instance of Beamable
data UserT f =
  User {
    _userId :: Columnar f Int
  , _userEmail :: Columnar f Text
  , _userFirstName :: Columnar f Text
  , _userMiddleName :: Columnar (Nullable f) Text -- Example of a Nullable column
  , _userLastName :: Columnar f Text
  , _userPermaId :: Columnar f UUID
  } deriving (Generic, Beamable)

deriving instance Show User
deriving instance Eq User
deriving instance Show UserId
deriving instance Eq UserId

-- Tables should have a primary key. In this case we set the column
-- userId as the primary key.
instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Int)
    deriving (Generic, Beamable)
  primaryKey = UserId . _userId

-- Let's define some type synonyms so we don't have to work with UserT or Columnar directly.
-- Columnar is a type family defined such that for any x, Columnar Identity x = x.
-- This strategy is known as defunctionalization or higher-kinded data types.
type User = UserT Identity

-- Our primary key for UserT
type UserId = PrimaryKey UserT Identity

-- We serialise our type using a representation that is more front
-- end friendly, plus we do not want to really expose the names of
-- our internal fields.
instance ToJSON User
  where
    toJSON (User _ e fname mname lname _) =
      object ["email" .= e
            , "firstName" .= fname
            , "middleName" .= mname
            , "lastName" .= lname
            ]
