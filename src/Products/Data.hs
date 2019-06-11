module Products.Data (
    findAll
  , findById
) where

import Data.DB (storeDb, StoreDb (storeProducts), getConnection)
import Data.Text (Text)
import Database.Beam
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (runSelectReturningList)

import Products.Types (Product, ProductT (productPermaId))

findAll :: IO [Product]
findAll = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $ select $ all_ (storeProducts storeDb)

findById :: Text -> IO (Maybe Product)
findById pId = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $ do
        prod <- all_ (storeProducts storeDb)
        guard_ (val_ pId ==. productPermaId prod)
        return prod
