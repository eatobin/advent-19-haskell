module Lib (IntCodeStruct (..), makeIntcode, lookUpFromMemory) where

import qualified Data.IntMap.Strict as IM
import qualified Data.List.Split as S

type Pointer = Int

type Memory = IM.IntMap Int

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
  IntCode {pointer = pointerParam, memory = IM.fromList (makeMemoryAsList memoryAsCSVString)}

lookUpFromMemory :: IntCodeStruct -> Pointer -> Int
lookUpFromMemory intCode pointerParam =
  (memory intCode) IM.! pointerParam

opCode :: IntCodeStruct -> IntCodeStruct
opCode intCode = case action of
  1 -> IM.adjust succ 0 (memory intCode)
  _ -> intCode
  where
    action = memory intCode IM.! pointer intCode

-- address1 = memory intCode IM.! pointer intCode + 1
-- address2 = memory intCode IM.! pointer intCode + 2
-- address3 = memory intCode IM.! pointer intCode + 3

-- adjust :: (a -> a) -> Key -> IntMap a -> IntMap a

-- cylinder :: (RealFloat a) => a -> a -> a
-- cylinder r h =
--   sideArea + 2 * topArea
--   where
--     sideArea = 2 * pi * r * h
--     topArea = pi * r ^ 2

-- describeNumber :: Int -> String
-- describeNumber x = case x of
--   0 -> "The number is zero."
--   1 -> "The number is one."
--   n | n < 0 -> "The number is negative." -- Guards can be used within case alternatives
--   _ -> "The number is some other positive integer."
