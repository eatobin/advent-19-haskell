{-# LANGUAGE NamedFieldPuns #-}

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

import Control.Monad.Trans.State.Strict
  ( State,
    evalState,
    execState,
    runState,
    state,
  )
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

data IntCodeStruct
  = IntCodeStruct
  { pointer :: Pointer,
    memory :: Memory
  }
  deriving (Eq, Show)

data IntCodeStructAction
  = Add
  | Multiply
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
  let memoryAsIntList = map read (Split.splitOn "," memoryAsCSVStringParam)
   in Vec.fromList memoryAsIntList

updatedMemory :: Int -> Int -> Memory -> Memory
updatedMemory noun verb mem =
  nounish Vec.// [(2, verb)]
  where
    nounish = mem Vec.// [(1, noun)]

keyToKey :: IntCodeStruct -> PointerOffset -> Key
keyToKey intCode pointerOffsetParam =
  memory intCode Vec.! (pointer intCode + pointerOffsetParam)

pw :: IntCodeStruct -> PointerOffset -> Key
pw =
  keyToKey

pr :: IntCodeStruct -> PointerOffset -> Value
pr intCode pointerOffsetParam =
  memory intCode Vec.! keyToKey intCode pointerOffsetParam

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
  IntCodeStruct
    { pointer = pointer intcode + 4,
      memory =
        memory intcode
          Vec.// [ ( aParam instruction intcode,
                     cParam instruction intcode + bParam instruction intcode
                   )
                 ]
    }

multiply :: Instruction -> IntCodeStruct -> IntCodeStruct
multiply instruction intcode =
  IntCodeStruct
    { pointer = pointer intcode + 4,
      memory =
        memory intcode
          Vec.// [ ( aParam instruction intcode,
                     cParam instruction intcode * bParam instruction intcode
                   )
                 ]
    }

goIntCodeAdd, goIntCodeMultiply :: IntCodeStruct -> (IntCodeStructAction, IntCodeStruct)
goIntCodeAdd IntCodeStruct {pointer, memory} = (Add, IntCodeStruct {pointer = pointer + 1, memory})
goIntCodeMultiply IntCodeStruct {pointer, memory} = (Multiply, IntCodeStruct {pointer = pointer * 10, memory})

goIntCodeAddState, goIntCodeMultiplyState :: State IntCodeStruct IntCodeStructAction
goIntCodeAddState = state goIntCodeAdd
goIntCodeMultiplyState = state goIntCodeMultiply

runOpCode :: IntCodeStruct -> IntCodeStruct
runOpCode intCode =
  case instruction Map.! 'e' of
    1 -> runOpCode (add instruction intCode)
    2 -> runOpCode (multiply instruction intCode)
    9 -> intCode
    _ -> error "Instruction is not valid"
  where
    instruction = makeInstruction (memory intCode Vec.! pointer intCode)

-- New stuff begins

data TrafficLightState = Red | Yellow | Green
  deriving (Eq, Show)

data TrafficLightAction
  = IAmStoppingFromYellowToRed
  | IAmSlowingFromGreenToYellow
  | IAmGoingFromRedToGreen
  | ICannotGoFromRedToYellow
  | ICannotGoFromGreenToRed
  | ICannotGoFromYellowToGreen
  | IAmAlreadyGreen
  | IAmAlreadyYellow
  | IAmAlreadyRed
  deriving (Eq, Show)

goGreen, goYellow, goRed :: TrafficLightState -> (TrafficLightAction, TrafficLightState)
goGreen Red = (IAmGoingFromRedToGreen, Green)
goGreen Yellow = (ICannotGoFromYellowToGreen, Yellow)
goGreen Green = (IAmAlreadyGreen, Green)
goYellow Green = (IAmSlowingFromGreenToYellow, Yellow)
goYellow Red = (ICannotGoFromRedToYellow, Red)
goYellow Yellow = (IAmAlreadyYellow, Yellow)
goRed Yellow = (IAmStoppingFromYellowToRed, Red)
goRed Green = (ICannotGoFromGreenToRed, Green)
goRed Red = (IAmAlreadyRed, Red)

goGreenState, goYellowState, goRedState :: State TrafficLightState TrafficLightAction
goGreenState = state goGreen
goYellowState = state goYellow
goRedState = state goRed

greenToRedState :: State TrafficLightState [TrafficLightAction]
greenToRedState = do
  a1 <- goYellowState
  a2 <- goRedState
  return [a1, a2]

addToMultiplyState :: State IntCodeStruct [IntCodeStructAction]
addToMultiplyState = do
  a1 <- goIntCodeAddState
  a2 <- goIntCodeMultiplyState
  a3 <- goIntCodeAddState
  a4 <- goIntCodeMultiplyState
  return [a1, a2, a3, a4]

greenToGreenAgainState :: State TrafficLightState [TrafficLightAction]
greenToGreenAgainState = do
  a1 <- goYellowState
  a2 <- goRedState
  a3 <- goGreenState
  return [a1, a2, a3]

trafficLightMain :: IO ()
trafficLightMain =
  do
    print (runState greenToRedState Green)
    print (runState greenToRedState Yellow)
    print (runState greenToRedState Red)
    print (evalState greenToRedState Green)
    print (execState greenToRedState Green)

trafficLightMainAgain :: IO ()
trafficLightMainAgain =
  do
    print (runState greenToGreenAgainState Green)
    print (runState greenToGreenAgainState Yellow)
    print (runState greenToGreenAgainState Red)
    print (evalState greenToGreenAgainState Green)
    print (execState greenToGreenAgainState Green)

intCodeStructMain :: IO ()
intCodeStructMain =
  do
    print (runState addToMultiplyState IntCodeStruct {pointer = 1, memory = Vec.fromList [3, 2, 1]})

-- print (runState addToMultiplyState IntCodeStruct {pointer = 2, memory = Vec.fromList [33, 22, 11]})
-- print (evalState addToMultiplyState IntCodeStruct {pointer = 3, memory = Vec.fromList [333, 222, 111]})
-- print (execState addToMultiplyState IntCodeStruct {pointer = 4, memory = Vec.fromList [3333, 2222, 1111]})

-- λ> trafficLightMain
-- λ> trafficLightMainAgain

-- λ> runState greenToRedState Green
-- λ> execState greenToRedState Green
-- λ> evalState greenToRedState Green

-- λ> runState goRedState Yellow
-- λ> runState goRedState Green
-- λ> runState goRedState Red
