module Products.Data (
    findAll
  , findById
) where

import Data.DB (getConnection, PageSize, storeDb, StoreDb (storeProducts))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (runSelectReturningList)

import Products.Types (Product, ProductT (productPermaId))

findAll :: PageSize -> IO [Product]
findAll pageSize = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select $
      limit_ pageSize $
      all_ (storeProducts storeDb)

findById :: Text -> IO (Maybe Product)
findById pId = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $ do
        prod <- all_ (storeProducts storeDb)
        guard_ (val_ pId ==. productPermaId prod)
        return prod
