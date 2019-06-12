{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Users.Data (
    create
  , findAll
  , findById
) where

import Data.DB (getConnection, PageNum, PageSize, storeDb, StoreDb (storeUsers))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (runSelectReturningList)

-- We import all the User entity functions as we can possibly sort of any of them.
import Users.Types (User, UserT (..))

findAll :: PageSize -> PageNum -> IO [User]
findAll pageSize pageNum = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select $
      orderBy_ (\u -> (asc_ (userFirstName u), desc_ (userLastName u))) $
      limit_ pageSize $
      offset_ pageNum $
      all_ (storeUsers storeDb)

findById :: Text -> IO (Maybe User)
findById uId = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $ do
        user <- all_ (storeUsers storeDb)
        guard_ (val_ uId ==. userPermaId user)
        return user

-- Persist the user to the database, and then return the newly created user
-- which should also now have a unique identifier auto generated for the user.
create :: Text -> Text -> Maybe Text -> Text -> IO User
create emailAddress firstName middleName lastName = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $ do
    [user] <- runInsertReturningList $ insert (storeUsers storeDb) $
      insertExpressions [User default_ (val_ emailAddress) (val_ firstName) (val_ middleName) (val_ lastName) default_]

    return user
