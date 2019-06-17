module Orders.Data (
    create
  , findAll
  , findById
  , findByUser
  , remove
) where

import Data.DB (getConnection, PageNum, PageSize, storeDb, StoreDb (storeOrders, storeUsers))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (runSelectReturningList, delete)

import Orders.Types (Order, OrderT (..))

import Products.Types (Product)

import Users.Types (User, UserT (userPermaId))

-- With Beam on a left join if we do not have a value we have to use Maybe Order, this
-- can get ugly if our list is a combination of Nothings and Justs. To cleanup the
-- result set we use isJust to filter out the Nothings.
findAll :: PageSize -> PageNum -> IO [Order]
findAll pageSize pageNum = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select $
      limit_ pageSize $
      offset_ pageNum $
      all_ (storeOrders storeDb)

-- Retrieve a specific order from the database using its universal identifier.
-- As such a order may not exist we return a Maybe T.
findById :: Text -> IO (Maybe Order)
findById oId = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $ do
        o <- all_ (storeOrders storeDb)
        guard_ (val_ oId ==. orderPermaId o)
        return o

-- With Beam on a left join if we do not have a value we have to use Maybe Order, this
-- can get ugly if our list is a combination of Nothings and Justs. To cleanup the
-- result set we use isJust to filter out the Nothings.
findByUser :: User -> PageSize -> PageNum -> IO [Maybe Order]
findByUser user pageSize pageNum = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select $
      limit_ pageSize $
      offset_ pageNum $ do
        u <- all_ (storeUsers storeDb)
        guard_ (val_ (userPermaId user) ==. userPermaId u)
        order <- leftJoin_ (all_ (storeOrders storeDb))
                           (\order -> orderUser order `references_` u)
        guard_ (isJust_ order)
        return order

-- Persist the order to the database, and then return the newly created order
-- The order is also associated to the product and user
create :: User -> Product -> IO Order
create user prod = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $ do
    [order] <- runInsertReturningList $ insert (storeOrders storeDb) $
      insertExpressions [Order default_ (val_ $ pk user) (val_ $ pk prod) default_]

    return order

-- Delete the order from the database.
remove :: Order -> IO ()
remove order = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runDelete $ delete (storeOrders storeDb)
      (\o -> orderPermaId o ==. val_ (orderPermaId order))