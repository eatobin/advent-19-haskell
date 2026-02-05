module Lib (lookupResult1, lookupResult2, updatedMap, lookupResult3, case1) where

import qualified Data.Map as M
import qualified Data.Maybe as DM

hasRedHat :: M.Map Int Int
hasRedHat = M.fromList [(0, 0), (1, 11), (2, 22)]

-- Look up an existing key
lookupResult1 :: Maybe Int
lookupResult1 = M.lookup 0 hasRedHat -- Just 0

-- Look up a non-existing key
lookupResult2 :: Maybe Int
lookupResult2 = M.lookup 19 hasRedHat -- Nothing

updatedMap :: M.Map Int Int
updatedMap = M.insert 19 99 hasRedHat

-- Look up a non-existing key
lookupResult3 :: Int
lookupResult3 = DM.fromJust (M.lookup 19 updatedMap) -- 99

data Case
  = Case
  { description :: String,
    input :: Integer,
    expected :: Bool
  }
  deriving (Show)

case1 :: Case
case1 = Case {description = "456 Oak Ave", input = 42, expected = True}
