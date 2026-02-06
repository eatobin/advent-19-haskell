module Lib (lookupResult1, lookupResult2, updatedMap, lookupResult3, case1, goodResult, badResult, vvv) where

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

handleMaybe :: Maybe a -> b -> (a -> b) -> b
handleMaybe maybeValue defaultValue function =
  case maybeValue of
    Nothing -> defaultValue
    Just value -> function value

-- goodResult :: Int
-- goodResult = handleMaybe (Just 42) 99 (+2)

-- badResult :: Int
-- badResult = handleMaybe Nothing 99 (+2)

goodResult :: Int
goodResult = handleMaybe lookupResult1 99 (+ 2)

badResult :: Int
badResult = handleMaybe lookupResult2 99 (+ 2)

-- fromMaybe :: a -> Maybe a -> a
-- Example:
vvv :: String
vvv = DM.fromMaybe "Hi" (Just "Hello") -- returns "Hello"
-- fromMaybe "Hi" Nothing        -- returns "Hi"
