{-# LANGUAGE FlexibleContexts #-}

module Users.Data (
    findAll
  , findById
) where

import Data.DB (getConnection, PageNum, PageSize, storeDb, StoreDb (storeUsers))
import Data.Text (Text)
import Database.Beam
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
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $ do
        user <- all_ (storeUsers storeDb)
        guard_ (val_ uId ==. userPermaId user)
        return user
