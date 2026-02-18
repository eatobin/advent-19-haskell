module Lib (IntCodeStruct (..), makeIntcode, lookUpFromMemory) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.Split as S

type Pointer = Int

type Memory = IntMap.IntMap Int

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
  IntCode {pointer = pointerParam, memory = IntMap.fromList (makeMemoryAsList memoryAsCSVString)}

lookUpFromMemory :: IntCodeStruct -> Pointer -> Int
lookUpFromMemory intCode pointerParam =
  (memory intCode) IntMap.! pointerParam
