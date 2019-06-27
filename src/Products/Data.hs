module Products.Data (
    countProducts
  , create
  , findAll
  , findById
  , updateState
) where

import Data.DB (getConnection, storeDb, StoreDb (storeProducts))
import Data.Text (Text)
import Database.Beam
import Database.Beam.Backend.SQL.BeamExtensions (runInsertReturningList, runUpdateReturningList)
import Database.Beam.Postgres (runBeamPostgresDebug)
import Database.Beam.Query (runSelectReturningList)

import Network.JSONApi

import Products.Types (Product, ProductT (..))

-- Retrieve all the users from the database restricting the number of Products
-- to the PageSize. Also return a specific page number. PageNum is a zero indexed
-- value.
findAll :: PageSize -> PageNum -> IO [Product]
findAll (PageSize pageSize) (PageNum pageNum) = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningList $
      select $
        orderBy_ (asc_ . productDescription) $
        limit_ (toInteger pageSize) $
        offset_ (toInteger $ pageNum * pageSize) $
        all_ (storeProducts storeDb)

-- Retrieve a specific product from the database using its universal identifier.
-- As such a product may not exist we return a Maybe T.
findById :: Text -> IO (Maybe Product)
findById pId = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $ do
        prod <- all_ (storeProducts storeDb)
        guard_ (val_ pId ==. productPermaId prod)
        return prod

{- |
Counts the total number of products in the database via the Beam aggregate function.
-}
countProducts :: IO (Maybe Int)
countProducts = do
  conn  <- getConnection
  runBeamPostgresDebug putStrLn conn $
    runSelectReturningOne $
      select $
        aggregate_ (const countAll_) (all_ (storeProducts storeDb))

-- Persist the product to the database, and then return the newly created product
-- which should also now have a unique identifier auto generated for the product.
create :: Text -> Integer -> IO Product
create desc price = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $ do
    [prod] <- runInsertReturningList $ insert (storeProducts storeDb) $
      insertExpressions [Product default_ (val_ desc) (val_ price) default_]

    return prod

-- Update the product state based on what is currently stored in the Product record
updateState :: Product -> IO Product
updateState productToUpdate = do
  conn <- getConnection
  runBeamPostgresDebug putStrLn conn $ do
    [prod] <- runUpdateReturningList $ save (storeProducts storeDb) productToUpdate

    return prod