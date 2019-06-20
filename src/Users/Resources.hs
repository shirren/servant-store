module Users.Resources (
    UserResource (..)
  , toResource
  ) where

import           Data.Aeson.TH
import           Data.Text (Text)

import           Network.JSONApi (ResourcefulEntity (..))
import qualified Network.JSONApi as JSONApi

data UserResource = UserResource
  { resourceId        :: Text
  , emailAddress :: Text
  , firstName :: Text
  , middleName :: Maybe Text
  , lastName  :: Text
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''UserResource)

instance ResourcefulEntity UserResource where
  resourceIdentifier = resourceId
  resourceType _ = "users"
  resourceLinks = Just . JSONApi.showLink
  resourceMetaData _ = Nothing
  resourceRelationships _ = Nothing
