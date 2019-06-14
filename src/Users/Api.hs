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

import Data.DB (defaultPageNum, defaultPageSize, PageNum, PageSize)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Servant ((:>), (:<|>)(..), Capture, Get, Handler, JSON, Post, ReqBody, Server, err404, throwError, QueryParam)

import Users.Data (create, findAll, findById)
import Users.Types (NewUserRequest (..), User)

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type UserApi =
  "api" :> "v1" :> "users" :>
  (
    QueryParam "page[size]" PageSize :> QueryParam "page[number]" PageNum :> Get '[JSON] [User] -- i.e. HTTP GET /api/v1/users
  :<|> Capture "id" Text :> Get '[JSON] User -- i.e. HTTP GET /api/v1/users/:id
  :<|> ReqBody '[JSON] NewUserRequest :> Post '[JSON] User -- i.e. HTTP POST /api/v1/users
  )

-- Definition of our User module API which maps our routes from the type
-- UserApi to a collection of functions that return a type of Handler.
usersServer :: Server UserApi
usersServer =
  getUsers :<|>
  getUser :<|>
  createUser

-- findAll returns type IO [User] which we lift to Handler [User]
getUsers :: Maybe PageSize -> Maybe PageNum -> Handler [User]
getUsers pageSize pageNum =
  liftIO $ findAll (fromMaybe defaultPageSize pageSize) (fromMaybe defaultPageNum pageNum)

getUser :: Text -> Handler User
getUser uId = do
  result <- liftIO $ findById uId
  case result of
    Just user -> pure user
    _         -> throwError err404

createUser :: NewUserRequest -> Handler User
createUser newUser =
  liftIO $ create (emailAddress newUser) (firstName newUser) (middleName newUser) (lastName newUser)