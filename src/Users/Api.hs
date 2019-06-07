{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- The API is the entry point into the module. Here we only export the definition
-- of our Api and the server. Note that the functions or "Handlers" are not exposed
-- thereby preserving encapsulation.
module Users.Api (
  UserApi,
  usersServer
) where

import           Data.List (find)
import           Data.Text (Text)

import           Servant

import qualified Users.Data as UsersDb
import           Users.Types (User, UserT (..))

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type UserApi =
  "api" :> "v1" :> "users" :>
  (
      Get '[JSON] [User] -- i.e. /api/v1/users
  :<|> Capture "id" Text :> Get '[JSON] User -- i.e. /api/v1/users/:id
  )

-- Definition of our User module API which maps our routes from the type
-- UserApi to a collection of functions that return a type of Handler.
usersServer :: Server UserApi
usersServer =
  getUsers :<|>
  getUser

getUsers :: Handler [User]
getUsers =
  pure UsersDb.findAll

getUser :: Text -> Handler User
getUser pId =
  case find (\ (User _ _ _ _ _ p) -> p == pId) UsersDb.findAll of
    Just user -> pure user
    _         -> throwError err404