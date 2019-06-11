{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- The API is the entry point into the module. Here we only export the definition
-- of our Api and the server. Note that the functions or "Handlers" are not exposed
-- thereby preserving encapsulation.
module Users.Api (
    UserApi
  , usersServer
) where

import Control.Monad.IO.Class (liftIO)

import Data.DB (defaultPageSize, PageSize)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Servant ((:>), (:<|>)(..), Capture, Get, Handler, JSON, Server, err404, throwError, QueryParam)

import Users.Data (findAll, findById)
import Users.Types (User)

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type UserApi =
  "api" :> "v1" :> "users" :>
  (
    QueryParam "page[size]" PageSize :> Get '[JSON] [User] -- i.e. /api/v1/users
  :<|> Capture "id" Text :> Get '[JSON] User -- i.e. /api/v1/users/:id
  )

-- Definition of our User module API which maps our routes from the type
-- UserApi to a collection of functions that return a type of Handler.
usersServer :: Server UserApi
usersServer =
  getUsers :<|>
  getUser

-- findAll returns type IO [User] which we lift to Handler [User]
getUsers :: Maybe PageSize -> Handler [User]
getUsers pageSize =
  liftIO $ findAll $ fromMaybe defaultPageSize pageSize

getUser :: Text -> Handler User
getUser uId = do
  result <- liftIO $ findById uId
  case result of
    Just user -> pure user
    _         -> throwError err404