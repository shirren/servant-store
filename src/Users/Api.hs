{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- The API is the entry point into the module. Here we only export the definition
-- of our Api and the server. Note that the functions or "Handlers" are not exposed
-- thereby preserving encapsulation.
module Users.Api (
  UserApi
, usersServer
) where

import Auth.ClaimsSubSet (ClaimsSubSet)
import Common.Paging
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Debug.Trace (trace)
import Network.JSONApi
import Servant (
    (:>)
  , (:<|>)(..)
  , Capture
  , Get
  , Handler
  , JSON
  , Post
  , PostCreated
  , ReqBody
  , Server
  , err401
  , err404
  , throwError
  , QueryParam)
import Servant.Auth.Server (AuthResult (Authenticated), throwAll)
import Users.Data (countUsers, create, findAll, findById, updateState)
import Users.Types (NewUserRequest (..), UpdateUserRequest (..), User, UserT (..))

import qualified Users.Resources as UR

-- | Example on how to define a nested route. The detailed route follows
-- a very common convention of nesting resources under /api/v1.
-- For a very good blog post on how to set these up please refer to
-- https://qfpl.io/posts/nested-routes-in-servant/
type UserApi =
  "api" :> "v1" :> "users" :>
  (
    QueryParam "page[size]" Int
      :> QueryParam "page[number]" Int
      :> Get '[JSON] (Document UR.UserResource) -- i.e. HTTP GET /api/v1/users
    :<|> Capture "id" Text
      :> Get '[JSON] (Document UR.UserResource) -- i.e. HTTP GET /api/v1/users/:id
    :<|> ReqBody '[JSON] NewUserRequest
      :> PostCreated '[JSON] (Document UR.UserResource) -- i.e. HTTP POST /api/v1/users
    :<|> Capture "id" Text
      :> ReqBody '[JSON] UpdateUserRequest
      :> Post '[JSON] (Document UR.UserResource) -- i.e. HTTP POST /api/v1/users/:id
  )

-- | Definition of our User module API which maps our routes from the type
-- UserApi to a collection of functions that return a type of Handler.
usersServer :: AuthResult ClaimsSubSet -> Server UserApi
usersServer (Authenticated _) =
  getUsers :<|>
  getUser :<|>
  createUser :<|>
  updateUser
usersServer c =
  trace (show c) (throwAll err401)

-- | FindAll returns type IO [User] which we lift to Handler (JSONApi.Document UR.UserResource)
-- which is [User] -> UserResource.
getUsers :: Maybe Int -> Maybe Int -> Handler (Document UR.UserResource)
getUsers pageSize pageNum = do
  users     <- liftIO $ findAll (wrapPgSize pageSize) (wrapPgNum pageNum)
  userCount <- liftIO countUsers
  let userResources = mkUserResource <$> users
  let pagination = Pagination (wrapPgSize pageSize) (wrapPgNum pageNum) (wrapResCount userCount)
  pure $ mkDocument userResources (Just $ indexLinks "/users" pagination) (Just $ mkMeta pagination)

-- | Retrieve the single user as a resource. If the user does not exist then we return
-- an Http 404 or not found.
getUser :: Text -> Handler (Document UR.UserResource)
getUser uId = do
  result <- liftIO $ findById uId
  case result of
    Just user -> pure $ mkSimpleDocument [mkUserResource user]
    _         -> throwError err404

-- | Register a new user in the store, and return this users details back to the client.
-- Why do we return the user? Well each user has a unique identifier, and this is serialised
-- back with the User in the event the client then wants to make a GET request to retrieve
-- this particular user.

-- Note that if the request does not conform to the shape of the
-- NewUserRequest Servant generates a 400 bad request. We do not need to handle this
-- error scenario.
createUser :: NewUserRequest -> Handler (Document UR.UserResource)
createUser newUser = do
  user <- liftIO $ create (emailAddress newUser) (firstName newUser) (middleName newUser) (lastName newUser)
  pure $ mkSimpleDocument [mkUserResource user]

-- | This endpoint allows a client to update user state. Note that even though the middle name is optional,
-- if the request does not include the middle name it is overwritten with Null in the database. This was
-- implemented like so for simplisity.
updateUser :: Text -> UpdateUserRequest -> Handler (Document UR.UserResource)
updateUser uId userData = do
  mUser <- liftIO $ findById uId
  case mUser of
    Just u -> do
      user <- liftIO $ updateState u {
          userFirstName = updatedFirstName userData
        , userMiddleName = updatedMiddleName userData
        , userLastName = updatedLastName userData }
      pure $ mkSimpleDocument [mkUserResource user]
    _      ->
      throwError err404

-- | Helper function that maps from UserT to UserResource
mkUserResource :: User -> UR.UserResource
mkUserResource user = UR.UserResource {
    UR.resourceId = userPermaId user
  , UR.emailAddress = userEmail user
  , UR.firstName = userFirstName user
  , UR.middleName = userMiddleName user
  , UR.lastName = userLastName user
  }
