{-# LANGUAGE FlexibleContexts #-}

module Users.Data (
    findAll
  , findById
) where

import Data.DB (getConnection, PageSize, storeDb, StoreDb (storeUsers))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (runSelectReturningList)

import Users.Types (User, UserT (userPermaId))

findAll :: PageSize -> IO [User]
findAll pageSize = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select $
      limit_ pageSize $
      all_ (storeUsers storeDb)

findById :: Text -> IO (Maybe User)
findById uId = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $ do
        user <- all_ (storeUsers storeDb)
        guard_ (val_ uId ==. userPermaId user)
        return user
