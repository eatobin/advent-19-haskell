module Lib (IntCodeStruct (..), makeInstruction, makeMemory, pw, pr, aParam, bParam, cParam) where

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

import qualified Data.Char as DC
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List.Split as S
import qualified Data.Map as Map

type Instruction = Map.Map Char Int

type Pointer = Int

type Key = Int

type Value = Int

type Memory = IntMap.IntMap Value

type MemoryAsCSVString = [Char]

type PointerOffset = Int

data IntCodeStruct
  = IntCode
  { pointer :: Pointer,
    memory :: Memory
  }
  deriving (Eq, Show)

pointerOffsetC :: PointerOffset
pointerOffsetC = 1

pointerOffsetB :: PointerOffset
pointerOffsetB = 2

pointerOffsetA :: PointerOffset
pointerOffsetA = 3

makeInstruction :: Int -> Instruction
makeInstruction op =
  Map.fromList instructionAsKVTupleList
  where
    keys = ['a', 'b', 'c', 'd', 'e']
    opAsString = show op
    paddedOp = replicate (5 - length opAsString) '0' ++ opAsString
    values = map DC.digitToInt paddedOp
    instructionAsKVTupleList = zip keys values

makeMemory :: MemoryAsCSVString -> Memory
makeMemory memoryAsCSVStringParam =
  let memoryAsKVTupleList = zip [0 ..] (map read (S.splitOn "," memoryAsCSVStringParam))
   in IntMap.fromList memoryAsKVTupleList

keyToKey :: IntCodeStruct -> PointerOffset -> Key
keyToKey intCode pointerOffsetParam =
  memory intCode IntMap.! (pointer intCode + pointerOffsetParam)

pw :: IntCodeStruct -> PointerOffset -> Key
pw =
  keyToKey

pr :: IntCodeStruct -> PointerOffset -> Value
pr intCode pointerOffsetParam =
  memory intCode IntMap.! keyToKey intCode pointerOffsetParam

aParam :: Instruction -> IntCodeStruct -> Int
aParam instruction intcode =
  case instruction Map.! 'a' of
    0 -> pw intcode pointerOffsetA -- a-p-w
    _ -> error "Instruction is not valid"

bParam :: Instruction -> IntCodeStruct -> Int
bParam instruction intcode = case instruction Map.! 'b' of
  0 -> pr intcode pointerOffsetB -- b-p-r
  _ -> error "Instruction is not valid"

cParam :: Instruction -> IntCodeStruct -> Int
cParam instruction intcode = case instruction Map.! 'c' of
  0 -> pr intcode pointerOffsetC -- c-p-r
  _ -> error "Instruction is not valid"

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
