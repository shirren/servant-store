{-# LANGUAGE OverloadedStrings #-}

module Users.Data (
  findAll
) where

import Database.Beam

import Users.Types (User (..), UserT (..))

findAll :: [User]
findAll = [ User 1 "isaac@newton.co.uk" "Isaac" "Newton" "1"
  , User 2 "ae@mc2.org" "Albert" "Einstein" "2"]