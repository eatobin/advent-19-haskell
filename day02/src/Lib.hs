module Lib (values, memoryAsList, intCode, lookupResult3, lookupResult4) where

import qualified Data.List.Split as S
import qualified Data.Map as M
import qualified Data.Maybe as DM

type Memory = M.Map Int Int

data IntCode
  = IntCode
  { pointer :: Int,
    memory :: Memory
  }
  deriving (Show)

memoryAsCSVString :: [Char]
memoryAsCSVString = "10,11,12,13,14"

memoryAsListOfSrings :: [[Char]]
memoryAsListOfSrings = S.splitOn "," memoryAsCSVString

myReadToInt :: String -> Int
myReadToInt = read

values :: [Int]
values = map myReadToInt memoryAsListOfSrings

memoryAsList :: [(Int, Int)]
memoryAsList = zip [0 ..] values

intCode :: IntCode
intCode = IntCode {pointer = 20, memory = M.fromList memoryAsList}

lookupResult3 :: Int
lookupResult3 = DM.fromJust (M.lookup 10 (memory intCode))

lookupResult4 :: Int
lookupResult4 = DM.fromJust (M.lookup 1 (memory intCode))
