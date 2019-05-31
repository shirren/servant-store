{-# LANGUAGE OverloadedStrings #-}

module Users.Data (
  findAll
) where

import Users.Types (User (..))

findAll :: [User]
findAll = [ User 1 "Isaac Newton"    "isaac@newton.co.uk"
  , User 2 "Albert Einstein" "ae@mc2.org"]