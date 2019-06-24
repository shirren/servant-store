{- |
JSON API lib paging types don't seem to play well with Servant. These functions are provided to
load these types in and out of their underlying primitive.
-}
module Common.Paging (
    wrapPgNum
  , wrapPgSize
  , wrapResCount
) where

import Data.Maybe (fromMaybe)

import Network.JSONApi

wrapPgSize :: Maybe Int -> PageSize
wrapPgSize org =
  PageSize (fromMaybe 10 org)

wrapPgNum :: Maybe Int -> PageNum
wrapPgNum org =
  PageNum (fromMaybe 0 org)

wrapResCount :: Maybe Int -> ResourceCount
wrapResCount org =
  ResourceCount (fromMaybe 0 org)
