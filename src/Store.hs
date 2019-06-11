{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Store (
  seedData
) where

import Users.Data (findAll)

-- Seed some baseline data in the application. System does not check to see if the data pre-exists
-- in the system.
seedData :: IO ()
seedData = do
  users <- findAll 10
  -- conn <- getConnection
  -- (user1, user2, product1, product2, order1) <- runBeamPostgresDebug putStrLn {- for debug output -} conn $ do
  --   [user1, user2] <- runInsertReturningList $ insert (_storeUsers storeDb) $
  --     insertExpressions [ User default_ "james@example.com" "James" nothing_ "Smith" default_
  --                       , User default_ "john@example.com" "John" (just_ "Adrian") "Doe" default_]

  --   [product1, product2] <- runInsertReturningList $ insert (_storeProducts storeDb) $
  --     insertExpressions [ Product default_ "Toothbrush" 350 default_
  --                       , Product default_ "Dental floss" 120 default_ ]

  --   [order1] <- runInsertReturningList $ insert (_storeOrders storeDb) $
  --     insertExpressions [ Order default_ (val_ $ pk user1) (val_ $ pk product1) default_ ]

  --   users <- findAll (_storeUsers storeDb)
  --   pure (user1, user2, product1, product2, order1)

  putStrLn $ show users
  -- putStrLn ("Seeding complete..." <> show user1 <> show user2 <> show product1 <> show product2 <> show order1)
