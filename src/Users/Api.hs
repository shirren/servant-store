{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Users.Api (
  UserApi,
  usersServer
) where

import           Data.List (find)

import           Servant

import qualified Users.Data as UsersDb
import           Users.Types (User (..))

type UserApi =
  "users" :> Get '[JSON] [User] :<|>
  "users" :> Capture "id" Int :> Get '[JSON] User

usersServer :: Server UserApi
usersServer =
  getUsers :<|>
  getUser

getUsers :: Handler [User]
getUsers =
  pure UsersDb.findAll

getUser :: Int -> Handler User
getUser id =
  case find (\ u -> userId u == id) UsersDb.findAll of
    Just user -> pure user
    _         -> throwError err404