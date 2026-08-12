module Lib (IntCode (..), IntCodeAction (..), Memory, makeThisMemory, makeCandidatePairs, findWinner, runOpCode, makeInstruction, makeMemory, updatedMemory, pw, pr, aParam, bParam, cParam, add, multiply, exit) where

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

import Control.Monad.Trans.State (State, execState, get, put)
import qualified Data.Char as DC
import qualified Data.List as DL
import qualified Data.List.Split as Split
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as DM
import qualified Data.Vector as Vec

type Instruction = Map.Map Char Int

type Pointer = Int

type Key = Int

type Value = Int

type Memory = Vec.Vector Int

type MemoryAsCSVString = [Char]

type PointerOffset = Int

type IntCodeState a = State IntCode a

type Actions = [IntCodeAction]

type Candidate = Int

type CandidatePair = (Candidate, Candidate)

type CandidatePairList = [CandidatePair]

type Winner = Int

type PairAndWinner = (CandidatePair, Winner)

type PairAndWinnerList = [(CandidatePair, Winner)]

data IntCode
  = IntCode
  { pointer :: Pointer,
    memory :: Memory,
    actions :: Actions
  }
  deriving (Eq, Show)

data IntCodeAction = Add | Multiply | Exit
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

keyToKey :: IntCode -> PointerOffset -> Key
keyToKey intCode pointerOffsetParam =
  memory intCode Vec.! (pointer intCode + pointerOffsetParam)

pw :: IntCode -> PointerOffset -> Key
pw =
  keyToKey

pr :: IntCode -> PointerOffset -> Value
pr intCode pointerOffsetParam =
  memory intCode Vec.! keyToKey intCode pointerOffsetParam

aParam :: Instruction -> IntCode -> Key
aParam instruction intCode =
  case instruction Map.! 'a' of
    0 -> pw intCode pointerOffsetA -- a-p-w
    _ -> error "Instruction A is not valid"

bParam :: Instruction -> IntCode -> Value
bParam instruction intCode =
  case instruction Map.! 'b' of
    0 -> pr intCode pointerOffsetB -- b-p-r
    _ -> error "Instruction B is not valid"

cParam :: Instruction -> IntCode -> Value
cParam instruction intCode =
  case instruction Map.! 'c' of
    0 -> pr intCode pointerOffsetC -- c-p-r
    _ -> error "Instruction C is not valid"

add :: Instruction -> IntCode -> IntCode
add instruction intCode =
  IntCode
    { pointer = pointer intCode + 4,
      memory =
        memory intCode
          Vec.// [ ( aParam instruction intCode,
                     cParam instruction intCode + bParam instruction intCode
                   )
                 ],
      actions = Add : actions intCode
    }

multiply :: Instruction -> IntCode -> IntCode
multiply instruction intCode =
  IntCode
    { pointer = pointer intCode + 4,
      memory =
        memory intCode
          Vec.// [ ( aParam instruction intCode,
                     cParam instruction intCode * bParam instruction intCode
                   )
                 ],
      actions = Multiply : actions intCode
    }

exit :: IntCode -> IntCode
exit intCode =
  IntCode
    { pointer = pointer intCode,
      memory = memory intCode,
      actions = Exit : actions intCode
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
    9 -> do
      put (exit currentState)
    _ -> error "Instruction runOpCode is not valid"

makeThisMemory :: String -> Memory
makeThisMemory = makeMemory

makeCandidatePairs :: CandidatePairList
makeCandidatePairs =
  [(noun, verb) | noun <- [0 .. (99 :: Int)], verb <- [0 .. (99 :: Int)]]

runAcandidatePair :: Memory -> CandidatePair -> PairAndWinner
runAcandidatePair thisMemory candidatePair =
  let candidateIntCode =
        execState runOpCode (IntCode {pointer = 0, memory = uncurry updatedMemory candidatePair thisMemory, actions = []})
   in (candidatePair, memory candidateIntCode Vec.! 0)

mapOverPairs :: CandidatePairList -> Memory -> PairAndWinnerList
mapOverPairs pairs thisMemory =
  map (runAcandidatePair thisMemory) pairs

winnerIs :: PairAndWinner -> Bool
winnerIs candidate =
  let ((_, _), calculation) = candidate
   in calculation == 19690720

findWinner :: CandidatePairList -> Memory -> PairAndWinner
findWinner pairs thisMemory = DM.fromJust (DL.find winnerIs (mapOverPairs pairs thisMemory))
