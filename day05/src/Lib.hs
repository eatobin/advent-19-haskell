module Lib (IntCodeStruct (..), Memory, makeInstruction, makeMemory, pw, pr, aParam, bParam, cParam, add, multiply, runOpCode) where

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

type KeyOrValue = Int

type Memory = Map.Map Key Value

type MemoryAsCSVString = [Char]

type PointerOffset = Int

type Input = Int

type Output = Int

data IntCodeStruct
  = IntCode
  { input :: Input,
    output :: Output,
    pointer :: Pointer,
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

readABC :: IntCodeStruct -> PointerOffset -> KeyOrValue
readABC intcode pointerOffsetParam =
  case result of
    Just value -> value
    Nothing -> error "Key is not valid"
  where
    result = Map.lookup (pointer intcode + pointerOffsetParam) (memory intcode)

pw :: IntCodeStruct -> PointerOffset -> Key
pw =
  readABC

pr :: IntCodeStruct -> PointerOffset -> Value
pr intcode pointerOffsetParam =
  memory intcode Map.! readABC intcode pointerOffsetParam

ir :: IntCodeStruct -> PointerOffset -> Value
ir =
  readABC

aParam :: Instruction -> IntCodeStruct -> Key
aParam instruction intcode =
  case instruction Map.! 'a' of
    0 -> pw intcode pointerOffsetA -- a-p-w
    _ -> error "Instruction is not valid"

bParam :: Instruction -> IntCodeStruct -> KeyOrValue
bParam instruction intcode =
  case instruction Map.! 'b' of
    0 -> pr intcode pointerOffsetB -- b-p-r
    1 -> ir intcode pointerOffsetB -- b-i-r
    _ -> error "Instruction is not valid"

cParam :: Instruction -> IntCodeStruct -> KeyOrValue
cParam instruction intcode =
  if instruction Map.! 'e' == 3
    then case instruction Map.! 'c' of
      0 -> pw intcode pointerOffsetC -- c-p-w
      _ -> error "Instruction is not valid"
    else case instruction Map.! 'c' of
      0 -> pr intcode pointerOffsetC -- c-p-r
      1 -> ir intcode pointerOffsetC -- c-i-r
      _ -> error "Instruction is not valid"

add :: Instruction -> IntCodeStruct -> IntCodeStruct
add instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      pointer = pointer intcode + 4,
      memory =
        Map.insert
          (aParam instruction intcode)
          (cParam instruction intcode + bParam instruction intcode)
          (memory intcode)
    }

multiply :: Instruction -> IntCodeStruct -> IntCodeStruct
multiply instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      pointer = pointer intcode + 4,
      memory =
        Map.insert
          (aParam instruction intcode)
          (cParam instruction intcode * bParam instruction intcode)
          (memory intcode)
    }

takeInput :: Instruction -> IntCodeStruct -> IntCodeStruct
takeInput instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      pointer = pointer intcode + 2,
      memory =
        Map.insert
          (cParam instruction intcode)
          (input intcode)
          (memory intcode)
    }

giveOutput :: Instruction -> IntCodeStruct -> IntCodeStruct
giveOutput instruction intcode =
  IntCode
    { input = input intcode,
      output = cParam instruction intcode,
      pointer = pointer intcode + 2,
      memory = memory intcode
    }

jumpIfTrue :: Instruction -> IntCodeStruct -> IntCodeStruct
jumpIfTrue instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      pointer =
        if cParam instruction intcode /= 0
          then bParam instruction intcode
          else pointer intcode + 3,
      memory = memory intcode
    }

jumpIfFalse :: Instruction -> IntCodeStruct -> IntCodeStruct
jumpIfFalse instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      pointer =
        if cParam instruction intcode == 0
          then bParam instruction intcode
          else pointer intcode + 3,
      memory = memory intcode
    }

lessThan :: Instruction -> IntCodeStruct -> IntCodeStruct
lessThan instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      pointer = pointer intcode + 4,
      memory =
        if cParam instruction intcode < bParam instruction intcode
          then
            Map.insert
              (aParam instruction intcode)
              1
              (memory intcode)
          else
            Map.insert
              (aParam instruction intcode)
              0
              (memory intcode)
    }

equals :: Instruction -> IntCodeStruct -> IntCodeStruct
equals instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      pointer = pointer intcode + 4,
      memory =
        if cParam instruction intcode == bParam instruction intcode
          then
            Map.insert
              (aParam instruction intcode)
              1
              (memory intcode)
          else
            Map.insert
              (aParam instruction intcode)
              0
              (memory intcode)
    }

runOpCode :: IntCodeStruct -> IntCodeStruct
runOpCode intcode =
  case instruction Map.! 'e' of
    1 -> runOpCode (add instruction intcode)
    2 -> runOpCode (multiply instruction intcode)
    3 -> runOpCode (takeInput instruction intcode)
    4 -> runOpCode (giveOutput instruction intcode)
    5 -> runOpCode (jumpIfTrue instruction intcode)
    6 -> runOpCode (jumpIfFalse instruction intcode)
    7 -> runOpCode (lessThan instruction intcode)
    8 -> runOpCode (equals instruction intcode)
    9 -> intcode
    _ -> error "Instruction is not valid"
  where
    instruction = makeInstruction (memory intcode Map.! pointer intcode)
