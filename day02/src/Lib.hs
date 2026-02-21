module Lib (IntCodeStruct (..), makeIntcode, keyToKey) where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.Split as S

type Pointer = Int

type Key = Int

type Value = Int

type Memory = IntMap.IntMap Int

type MemoryAsCSVString = [Char]

type MemoryAsList = [(Key, Value)]

type PointerOffset = Int

data IntCodeStruct
  = IntCode
  { pointer :: Pointer,
    memory :: Memory
  }
  deriving (Eq, Show)

-- Instruction:
-- ABCDE
-- 01234
-- 01002
-- 34(DE) - two-digit opcode,      02 == opcode 2
-- 2(C) - mode of 1st parameter,  0 == position mode
-- 1(B) - mode of 2nd parameter,  1 == immediate mode
-- 0(A) - mode of 3rd parameter,  0 == position mode, omitted due to being a leading zero

-- 0 1 or 2 - left-to-right position after 2 digit opcode
-- p i or r - position, immediate or relative mode
-- r or w - read or write

-- pointerOffsetC :: PointerOffset
-- pointerOffsetC = 1

-- pointerOffsetB :: PointerOffset
-- pointerOffsetB = 2

-- pointerOffsetA :: PointerOffset
-- pointerOffsetA = 3

myReadToInt :: String -> Int
myReadToInt = read

makeMemoryAsList :: MemoryAsCSVString -> MemoryAsList
makeMemoryAsList memoryAsCSVString =
  zip [0 ..] (map myReadToInt (S.splitOn "," memoryAsCSVString))

makeIntcode :: Pointer -> MemoryAsCSVString -> IntCodeStruct
makeIntcode pointerParam memoryAsCSVString =
  IntCode {pointer = pointerParam, memory = IntMap.fromList (makeMemoryAsList memoryAsCSVString)}

-- lookUpFromMemory :: IntCodeStruct -> Key -> Value
-- lookUpFromMemory intCode index =
--   memory intCode IntMap.! index

keyToKey :: IntCodeStruct -> PointerOffset -> Key
keyToKey intCode pointerOffsetParam =
  memory intCode IntMap.! (pointer intCode + pointerOffsetParam)

-- pw :: IntCodeStruct -> PointerOffset -> Key
-- pw intCode pointerOffsetParam =
--   keyToKey intCode pointerOffsetParam

-- pr :: IntCodeStruct -> PointerOffset -> Value
-- pr intCode pointerOffsetParam =
--   memory intCode IntMap.! keyToKey intCode pointerOffsetParam

-- opCode :: IntCodeStruct -> IntCodeStruct
-- opCode intCode = case action of
--   1 -> IntMap.adjust succ 0 (memory intCode)
--   _ -> intCode
--   where
--     action = memory intCode IntMap.! pointer intCode

-- address1 = memory intCode IntMap.! pointer intCode + 1
-- address2 = memory intCode IntMap.! pointer intCode + 2
-- address3 = memory intCode IntMap.! pointer intCode + 3

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
