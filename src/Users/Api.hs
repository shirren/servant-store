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

import Servant ((:>), (:<|>)(..), Capture, Get, Handler, JSON, Post, PostCreated, ReqBody, Server, err404, throwError, QueryParam)

import Users.Data (create, findAll, findById, updateState)
import Users.Types (NewUserRequest (..), UpdateUserRequest (..), User, UserT (..))

-- Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1. For a
-- a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type UserApi =
  "api" :> "v1" :> "users" :>
  (
    QueryParam "page[size]" PageSize :> QueryParam "page[number]" PageNum :> Get '[JSON] [User] -- i.e. HTTP GET /api/v1/users
  :<|> Capture "id" Text :> Get '[JSON] User -- i.e. HTTP GET /api/v1/users/:id
  :<|> ReqBody '[JSON] NewUserRequest :> PostCreated '[JSON] User -- i.e. HTTP POST /api/v1/users
  :<|> Capture "id" Text :> ReqBody '[JSON] UpdateUserRequest :> Post '[JSON] User -- i.e. HTTP POST /api/v1/users/:id
  )

-- Definition of our User module API which maps our routes from the type
-- UserApi to a collection of functions that return a type of Handler.
usersServer :: Server UserApi
usersServer =
  getUsers :<|>
  getUser :<|>
  createUser :<|>
  updateUser

-- findAll returns type IO [User] which we lift to Handler [User]
getUsers :: Maybe PageSize -> Maybe PageNum -> Handler [User]
getUsers pageSize pageNum =
  liftIO $ findAll (fromMaybe defaultPageSize pageSize) (fromMaybe defaultPageNum pageNum)

-- Retrieve the single user as a resource. If the user does not exist then we return
-- an Http 404 or not found.
getUser :: Text -> Handler User
getUser uId = do
  result <- liftIO $ findById uId
  case result of
    Just user -> pure user
    _         -> throwError err404

-- Register a new user in the store, and return this users details back to the client.
-- Why do we return the user? Well each user has a unique identifier, and this is serialised
-- back with the User in the event the client then wants to make a GET request to retrieve
-- this particular user. Note that if the request does not conform to the shape of the
-- NewUserRequest Servant generates a 400 bad request. We do not need to handle this
-- error scenario.
createUser :: NewUserRequest -> Handler User
createUser newUser =
  liftIO $ create (emailAddress newUser) (firstName newUser) (middleName newUser) (lastName newUser)

-- This endpoint allows a client to update user state. Note that even though the middle name is optional,
-- if the request does not include the middle name it is overwritten with Null in the database. This was
-- implemented like so for simplisity.
updateUser :: Text -> UpdateUserRequest -> Handler User
updateUser uId userData = do
  mUser <- liftIO $ findById uId
  case mUser of
    Just u ->
      liftIO $ updateState u {
          userFirstName = updatedFirstName userData
        , userMiddleName = updatedMiddleName userData
        , userLastName = updatedLastName userData }
    _      ->
      throwError err404