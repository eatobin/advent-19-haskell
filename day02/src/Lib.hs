module Lib () where

-- module Lib (lookupResult1, lookupResult2, updatedMap, lookupResult3, case1, goodResult, badResult, vvv) where

import qualified Data.Map as M
import qualified Data.Maybe as DM

type Memory = M.Map Int Int

data IntCode
  = IntCode
  { pointer :: Int,
    memory :: Memory
  }
  deriving (Show)

keys :: [Int]
keys = [0 .. 50]

values :: [Int]
values = [0, 11, 22, 33]

memoryAsList :: [(Int, Int)]
memoryAsList = zip keys values

intCode :: IntCode
intCode = IntCode {pointer = 2, memory = M.fromList memoryAsList}

-- memory :: Memory
-- memory = M.fromList memoryAsList

-- hasRedHat :: M.Map Int Int
-- hasRedHat = M.fromList [(0, 0), (1, 11), (2, 22)]

-- Look up an existing key
-- lookupResult1 :: Maybe Int
-- lookupResult1 = M.lookup 3 intCode.memory -- Just 0

-- -- Look up a non-existing key
-- lookupResult2 :: Maybe Int
-- lookupResult2 = M.lookup 19 memory -- Nothing

-- updatedMap :: M.Map Int Int
-- updatedMap = M.insert 19 99 memory

-- lookupResult3 :: Int
-- lookupResult3 = DM.fromJust (M.lookup 19 updatedMap) -- 99

-- case1 :: Case
-- case1 = Case {description = "456 Oak Ave", input = 42, expected = True}

-- handleMaybe :: Maybe a -> b -> (a -> b) -> b
-- handleMaybe maybeValue defaultValue function =
--   case maybeValue of
--     Nothing -> defaultValue
--     Just value -> function value

-- -- goodResult :: Int
-- -- goodResult = handleMaybe (Just 42) 99 (+2)

-- -- badResult :: Int
-- -- badResult = handleMaybe Nothing 99 (+2)

-- goodResult :: Int
-- goodResult = handleMaybe lookupResult1 99 (+ 2)

-- badResult :: Int
-- badResult = handleMaybe lookupResult2 99 (+ 2)

-- -- fromMaybe :: a -> Maybe a -> a
-- -- Example:
-- vvv :: String
-- vvv = DM.fromMaybe "Hi" (Just "Hello") -- returns "Hello"
-- -- fromMaybe "Hi" Nothing        -- returns "Hi"
