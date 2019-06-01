{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Users.Api (
  UserApi,
  usersServer
) where

import           Data.List (find)
import           Data.Text (Text)

import           Servant

import qualified Users.Data as UsersDb
import           Users.Types (User (..), UserT (..))

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

usersServer :: Server UserApi
usersServer =
  getUsers :<|>
  getUser

getUsers :: Handler [User]
getUsers =
  pure UsersDb.findAll

getUser :: Text -> Handler User
getUser id =
  case find (\ (User e f l p) -> p == id) UsersDb.findAll of
    Just user -> pure user
    _         -> throwError err404