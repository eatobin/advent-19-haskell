module Lib (IntCode (..), Memory, makeInstruction, makeMemory, updatedMemory, pw, pr, runOpCode, aParam, bParam, cParam, add, multiply) where

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

import Control.Monad.Trans.State (State, get, put)
import qualified Data.Char as DC
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Vector as Vec

type Instruction = Map.Map Char Int

type Pointer = Int

type Key = Int

type Value = Int

type Memory = Vec.Vector Int

type MemoryAsCSVString = [Char]

type PointerOffset = Int

data IntCode
  = IntCode
  { pointer :: Pointer,
    memory :: Memory
  }
  deriving (Eq, Show)

data IntCodeAction = Add | Multiply | Done
  deriving (Eq, Show)

type IntCodeState a = State IntCode a

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
  let memoryAsIntList = map read (Split.splitOn "," memoryAsCSVStringParam)
   in Vec.fromList memoryAsIntList

updatedMemory :: Int -> Int -> Memory -> Memory
updatedMemory noun verb mem =
  nounish Vec.// [(2, verb)]
  where
    nounish = mem Vec.// [(1, noun)]

keyToKey :: IntCode -> PointerOffset -> Key
keyToKey intCode pointerOffsetParam =
  memory intCode Vec.! (pointer intCode + pointerOffsetParam)

pw :: IntCode -> PointerOffset -> Key
pw =
  keyToKey

pr :: IntCode -> PointerOffset -> Value
pr intCode pointerOffsetParam =
  memory intCode Vec.! keyToKey intCode pointerOffsetParam

aParam :: Instruction -> IntCode -> Int
aParam instruction intcode =
  case instruction Map.! 'a' of
    0 -> pw intcode pointerOffsetA -- a-p-w
    _ -> error "Instruction is not valid"

bParam :: Instruction -> IntCode -> Int
bParam instruction intcode =
  case instruction Map.! 'b' of
    0 -> pr intcode pointerOffsetB -- b-p-r
    _ -> error "Instruction is not valid"

cParam :: Instruction -> IntCode -> Int
cParam instruction intcode =
  case instruction Map.! 'c' of
    0 -> pr intcode pointerOffsetC -- c-p-r
    _ -> error "Instruction is not valid"

add :: Instruction -> IntCode -> IntCode
add instruction intcode =
  IntCode
    { pointer = pointer intcode + 4,
      memory =
        memory intcode
          Vec.// [ ( aParam instruction intcode,
                     cParam instruction intcode + bParam instruction intcode
                   )
                 ]
    }

multiply :: Instruction -> IntCode -> IntCode
multiply instruction intcode =
  IntCode
    { pointer = pointer intcode + 4,
      memory =
        memory intcode
          Vec.// [ ( aParam instruction intcode,
                     cParam instruction intcode * bParam instruction intcode
                   )
                 ]
    }

runOpCode :: IntCodeState ()
runOpCode = do
  currentState <- get
  let instruction = makeInstruction (memory currentState Vec.! pointer currentState)
  case instruction Map.! 'e' of
    1 -> do
      put (add instruction currentState)
      runOpCode
    2 -> do
      put (multiply instruction currentState)
      runOpCode
    9 -> do put currentState
    _ -> error "Instruction is not valid"
