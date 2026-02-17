module Lib (IntCodeStruct (..), makeIntcode) where

import qualified Data.List.Split as S
import qualified Data.Map as M

-- import qualified Data.Maybe as DM

type Pointer = Int

type Memory = M.Map Int Int

type MemoryAsCSVString = [Char]

type MemoryAsList = [(Int, Int)]

data IntCodeStruct
  = IntCode
  { pointer :: Pointer,
    memory :: Memory
  }
  deriving (Eq, Show)

myReadToInt :: String -> Int
myReadToInt = read

makeMemoryAsList :: MemoryAsCSVString -> MemoryAsList
makeMemoryAsList memoryAsCSVString =
  zip [0 ..] (map myReadToInt (S.splitOn "," memoryAsCSVString))

makeIntcode :: Pointer -> MemoryAsCSVString -> IntCodeStruct
makeIntcode pointerParam memoryAsCSVString =
  IntCode {pointer = pointerParam, memory = M.fromList (makeMemoryAsList memoryAsCSVString)}

-- lookUpFromMemory :: IntCode -> Int
-- lookUpFromMemory intCode =
--   DM.fromJust (M.lookup (pointer intCode) (memory intCode))

-- memoryAsCSVString :: [Char]
-- memoryAsCSVString = "10,11,12,13,14"

-- values :: [Int]
-- values = S.splitOn "," memoryAsCSVString & map myReadToInt

-- memoryAsList :: [(Int, Int)]
-- memoryAsList = zip [0 ..] values

-- intCode :: IntCode
-- intCode = IntCode {pointer = 20, memory = M.fromList memoryAsList}

-- lookupResult3 :: Int
-- lookupResult3 = DM.fromJust (M.lookup 10 (memory intCode))

-- lookupResult4 :: Int
-- lookupResult4 = DM.fromJust (M.lookup 1 (memory intCode))
