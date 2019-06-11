module Orders.Data (
  findByUser
) where

import Data.DB (storeDb, StoreDb (storeOrders, storeUsers), getConnection)
import Database.Beam
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (runSelectReturningList)

import qualified Orders.Types as O

import Users.Types (User, UserT (userPermaId))

-- With Beam on a left join if we do not have a value we have to use Maybe Order, this
-- can get ugly if our list is a combination of Nothings and Justs. To cleanup the
-- result set we use isJust to filter out the Nothings.
findByUser :: User -> IO [Maybe O.Order]
findByUser user = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select $ do
        u <- all_ (storeUsers storeDb)
        guard_ (val_ (userPermaId user) ==. userPermaId u)
        order <- leftJoin_ (all_ (storeOrders storeDb))
                           (\order -> O.orderUser order `references_` u)
        guard_ (isJust_ order)
        return order