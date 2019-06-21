{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Users.Types (
    NewUserRequest (..)
  , UpdateUserRequest (..)
  , User
  , UserId
  , UserT (..)
) where

import Data.Aeson ((.:), (.:?), FromJSON, parseJSON, withObject)
import Data.Text (Text)
import Database.Beam (Beamable, Columnar, Identity, PrimaryKey, Nullable, Table, primaryKey)

import GHC.Generics (Generic)

-- Define a type that is representative of a table with columns
-- All types that need to be stored in the database need to
-- be an instance of Beamable
data UserT f =
  User {
    _userId :: Columnar f Int
  , userEmail :: Columnar f Text
  , userFirstName :: Columnar f Text
  , userMiddleName :: Columnar (Nullable f) Text -- Example of a Nullable column
  , userLastName :: Columnar f Text
  , userPermaId :: Columnar f Text
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

{- |
This type is used to represent the request body for a new User submitted
by a client to this API which would look something like;
{
   "firstName": "John"
 , "middleName" : "Adrian"
 , "lastName" : "Doe"
 , "emailAddress": "john@doe.com"
}
-}
data NewUserRequest = NewUserRequest {
  firstName :: Text
, middleName :: Maybe Text
, lastName :: Text
, emailAddress :: Text
} deriving (Generic)

instance FromJSON NewUserRequest

{- |
This type is used to represent the request body for a User update submitted
by a client to this API which would look something like;
{
   "firstName": "John"
 , "middleName" : "Adrian"
 , "lastName" : "Doe"
}
Note how the users email address cannot be updated
-}
data UpdateUserRequest = UpdateUserRequest {
  updatedFirstName :: Text
, updatedMiddleName :: Maybe Text
, updatedLastName :: Text
} deriving (Generic)

instance FromJSON UpdateUserRequest
  where
    parseJSON = withObject "UpdateUserRequest" $
      \o -> UpdateUserRequest <$> o .: "firstName"
                              <*> o .:? "middleName"
                              <*> o .: "lastName"