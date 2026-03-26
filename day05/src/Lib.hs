module Lib (IntCodeStruct (..), Memory, makeInstruction, makeMemory, updatedMemory, pw, pr, aParam, bParam, cParam, add, multiply, runOpCode) where

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
import qualified Data.List.Split as S
import qualified Data.Map.Strict as Map

type Instruction = Map.Map Char Int

type Pointer = Int

type Key = Int

type Value = Int

type Memory = Map.Map Key Value

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
   in Map.fromList memoryAsKVTupleList

updatedMemory :: Int -> Int -> Memory -> Memory
updatedMemory noun verb mem =
  Map.insert 2 verb nounish
  where
    nounish = Map.insert 1 noun mem

keyToKey :: IntCodeStruct -> PointerOffset -> Key
keyToKey intCode pointerOffsetParam =
  memory intCode Map.! (pointer intCode + pointerOffsetParam)

pw :: IntCodeStruct -> PointerOffset -> Key
pw =
  keyToKey

pr :: IntCodeStruct -> PointerOffset -> Value
pr intCode pointerOffsetParam =
  memory intCode Map.! keyToKey intCode pointerOffsetParam

aParam :: Instruction -> IntCodeStruct -> Int
aParam instruction intcode =
  case instruction Map.! 'a' of
    0 -> pw intcode pointerOffsetA -- a-p-w
    _ -> error "Instruction is not valid"

bParam :: Instruction -> IntCodeStruct -> Int
bParam instruction intcode =
  case instruction Map.! 'b' of
    0 -> pr intcode pointerOffsetB -- b-p-r
    _ -> error "Instruction is not valid"

cParam :: Instruction -> IntCodeStruct -> Int
cParam instruction intcode =
  case instruction Map.! 'c' of
    0 -> pr intcode pointerOffsetC -- c-p-r
    _ -> error "Instruction is not valid"

add :: Instruction -> IntCodeStruct -> IntCodeStruct
add instruction intcode =
  IntCode
    { pointer = pointer intcode + 4,
      memory =
        Map.insert
          (aParam instruction intcode)
          (cParam instruction intcode + bParam instruction intcode)
          (memory intcode)
    }

multiply :: Instruction -> IntCodeStruct -> IntCodeStruct
multiply instruction intcode =
  IntCode
    { pointer = pointer intcode + 4,
      memory =
        Map.insert
          (aParam instruction intcode)
          (cParam instruction intcode * bParam instruction intcode)
          (memory intcode)
    }

runOpCode :: IntCodeStruct -> IntCodeStruct
runOpCode intCode =
  case instruction Map.! 'e' of
    1 -> runOpCode (add instruction intCode)
    2 -> runOpCode (multiply instruction intCode)
    9 -> intCode
    _ -> error "Instruction is not valid"
  where
    instruction = makeInstruction (memory intCode Map.! pointer intCode)
