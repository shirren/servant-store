module Products.Data (
    create
  , findAll
  , findById
) where

import Data.DB (getConnection, PageNum, PageSize, storeDb, StoreDb (storeProducts))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList)
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (runSelectReturningList)

import Products.Types (Product, ProductT (..))

findAll :: PageSize -> PageNum -> IO [Product]
findAll pageSize pageNum = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select $
      limit_ pageSize $
      offset_ pageNum $
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

-- Persist the product to the database, and then return the newly created product
-- which should also now have a unique identifier auto generated for the product.
create :: Text -> Integer -> IO Product
create desc price = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $ do
    [prod] <- runInsertReturningList $ insert (storeProducts storeDb) $
      insertExpressions [Product default_ (val_ desc) (val_ price) default_]

    return prod