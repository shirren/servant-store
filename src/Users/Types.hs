{-# LANGUAGE DeriveGeneric #-}

module Users.Types (
  User (..)
) where

import Data.Aeson (ToJSON)
import Data.Text (Text)

import GHC.Generics (Generic)

data User = User {
  userId  :: Int
  , fullName :: Text
  , emailAddress :: Text
} deriving (Generic)

instance ToJSON User
