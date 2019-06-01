{-# LANGUAGE OverloadedStrings #-}

module Users.Data (
  findAll
) where

import Database.Beam

import Users.Types (User (..), UserT (..))

findAll :: [User]
findAll = [ User "isaac@newton.co.uk" "Isaac" "Newton" "1"
  , User "ae@mc2.org" "Albert" "Einstein" "2"]