module Lib (IntCodeStruct (..), runOpCode) where

-- Instruction = Map.Map Char Int:

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

-- [input output phase pointer memory stopped? recur?]

import qualified Data.Char as DC
import qualified Data.Map.Strict as Map

type Instruction = Map.Map Char Int

type Pointer = Int

type Key = Int

type Value = Int

type KeyOrValue = Int

type PointerOffset = Int

type Input = Int

type Output = Int

type Phase = Int

type Memory = Map.Map Key Value

type Stopped = Bool

type Recur = Bool

data IntCodeStruct
  = IntCode
  { input :: Input,
    output :: Output,
    phase :: Phase,
    pointer :: Pointer,
    memory :: Memory,
    stopped :: Stopped,
    recur :: Recur
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

lookupABC :: IntCodeStruct -> PointerOffset -> Maybe KeyOrValue
lookupABC intCode pointerOffsetParam =
  Map.lookup
    (pointer intCode + pointerOffsetParam)
    (memory intCode)

readABC :: IntCodeStruct -> PointerOffset -> KeyOrValue
readABC intCode pointerOffsetParam =
  case lookupABC intCode pointerOffsetParam of
    Just value -> value
    Nothing -> error "Key is not valid"

pw :: IntCodeStruct -> PointerOffset -> Key
pw =
  readABC

pr :: IntCodeStruct -> PointerOffset -> Value
pr intCode pointerOffsetParam =
  memory intCode Map.! readABC intCode pointerOffsetParam

ir :: IntCodeStruct -> PointerOffset -> Value
ir =
  readABC

aParam :: Instruction -> IntCodeStruct -> Key
aParam instruction intCode =
  case instruction Map.! 'a' of
    0 -> pw intCode pointerOffsetA -- a-p-w
    _ -> error "Instruction is not valid"

bParam :: Instruction -> IntCodeStruct -> KeyOrValue
bParam instruction intCode =
  case instruction Map.! 'b' of
    0 -> pr intCode pointerOffsetB -- b-p-r
    1 -> ir intCode pointerOffsetB -- b-i-r
    _ -> error "Instruction is not valid"

cParam :: Instruction -> IntCodeStruct -> KeyOrValue
cParam instruction intCode =
  if instruction Map.! 'e' == 3
    then case instruction Map.! 'c' of
      0 -> pw intCode pointerOffsetC -- c-p-w
      _ -> error "Instruction is not valid"
    else case instruction Map.! 'c' of
      0 -> pr intCode pointerOffsetC -- c-p-r
      1 -> ir intCode pointerOffsetC -- c-i-r
      _ -> error "Instruction is not valid"

add :: Instruction -> IntCodeStruct -> IntCodeStruct
add instruction intCode =
  IntCode
    { input = input intCode,
      output = output intCode,
      phase = phase intCode,
      pointer = pointer intCode + 4,
      memory =
        Map.insert
          (aParam instruction intCode)
          (cParam instruction intCode + bParam instruction intCode)
          (memory intCode),
      stopped = stopped intCode,
      recur = recur intCode
    }

multiply :: Instruction -> IntCodeStruct -> IntCodeStruct
multiply instruction intCode =
  IntCode
    { input = input intCode,
      output = output intCode,
      phase = phase intCode,
      pointer = pointer intCode + 4,
      memory =
        Map.insert
          (aParam instruction intCode)
          (cParam instruction intCode * bParam instruction intCode)
          (memory intCode),
      stopped = stopped intCode,
      recur = recur intCode
    }

takeInput :: Instruction -> IntCodeStruct -> IntCodeStruct
takeInput instruction intCode =
  IntCode
    { input = input intCode,
      output = output intCode,
      phase = phase intCode,
      pointer = pointer intCode + 2,
      memory =
        if phase intCode == (-1)
          then
            Map.insert
              (cParam instruction intCode)
              (input intCode)
              (memory intCode)
          else
            if pointer intCode == 0
              then
                Map.insert
                  (cParam instruction intCode)
                  (phase intCode)
                  (memory intCode)
              else
                Map.insert
                  (cParam instruction intCode)
                  (input intCode)
                  (memory intCode),
      stopped = stopped intCode,
      recur = recur intCode
    }

giveOutput :: Instruction -> IntCodeStruct -> IntCodeStruct
giveOutput instruction intCode =
  IntCode
    { input = input intCode,
      output = cParam instruction intCode,
      phase = phase intCode,
      pointer = pointer intCode + 2,
      memory = memory intCode,
      stopped = stopped intCode,
      recur = recur intCode
    }

jumpIfTrue :: Instruction -> IntCodeStruct -> IntCodeStruct
jumpIfTrue instruction intCode =
  IntCode
    { input = input intCode,
      output = output intCode,
      phase = phase intCode,
      pointer =
        if cParam instruction intCode /= 0
          then bParam instruction intCode
          else pointer intCode + 3,
      memory = memory intCode,
      stopped = stopped intCode,
      recur = recur intCode
    }

jumpIfFalse :: Instruction -> IntCodeStruct -> IntCodeStruct
jumpIfFalse instruction intCode =
  IntCode
    { input = input intCode,
      output = output intCode,
      phase = phase intCode,
      pointer =
        if cParam instruction intCode == 0
          then bParam instruction intCode
          else pointer intCode + 3,
      memory = memory intCode,
      stopped = stopped intCode,
      recur = recur intCode
    }

lessThan :: Instruction -> IntCodeStruct -> IntCodeStruct
lessThan instruction intCode =
  IntCode
    { input = input intCode,
      output = output intCode,
      phase = phase intCode,
      pointer = pointer intCode + 4,
      memory =
        if cParam instruction intCode < bParam instruction intCode
          then
            Map.insert
              (aParam instruction intCode)
              1
              (memory intCode)
          else
            Map.insert
              (aParam instruction intCode)
              0
              (memory intCode),
      stopped = stopped intCode,
      recur = recur intCode
    }

equals :: Instruction -> IntCodeStruct -> IntCodeStruct
equals instruction intCode =
  IntCode
    { input = input intCode,
      output = output intCode,
      phase = phase intCode,
      pointer = pointer intCode + 4,
      memory =
        if cParam instruction intCode == bParam instruction intCode
          then
            Map.insert
              (aParam instruction intCode)
              1
              (memory intCode)
          else
            Map.insert
              (aParam instruction intCode)
              0
              (memory intCode),
      stopped = stopped intCode,
      recur = recur intCode
    }

exit :: IntCodeStruct -> IntCodeStruct
exit intCode =
  IntCode
    { input = input intCode,
      output = output intCode,
      phase = phase intCode,
      pointer = pointer intCode,
      memory = memory intCode,
      stopped = True,
      recur = recur intCode
    }

runOpCode :: IntCodeStruct -> IntCodeStruct
runOpCode intCode =
  if stopped intCode
    then intCode
    else case instruction Map.! 'e' of
      1 -> runOpCode (add instruction intCode)
      2 -> runOpCode (multiply instruction intCode)
      3 -> runOpCode (takeInput instruction intCode)
      4 ->
        if recur intCode
          then
            runOpCode (giveOutput instruction intCode)
          else
            giveOutput instruction intCode
      5 -> runOpCode (jumpIfTrue instruction intCode)
      6 -> runOpCode (jumpIfFalse instruction intCode)
      7 -> runOpCode (lessThan instruction intCode)
      8 -> runOpCode (equals instruction intCode)
      9 -> runOpCode (exit intCode)
      _ -> error "Instruction is not valid"
  where
    instruction = makeInstruction (memory intCode Map.! pointer intCode)
