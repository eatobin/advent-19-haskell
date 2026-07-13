module Lib (IntCodeState (..), Memory, makeInstruction, makeMemory, updatedMemory, pw, pr, aParam, bParam, cParam, add, multiply, runOpCode) where

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

data IntCodeState
  = IntCodeState
  { pointer :: Pointer,
    memory :: Memory
  }
  deriving (Eq, Show)

data IntCodeAction = Add | Multiply
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

keyToKey :: IntCodeState -> PointerOffset -> Key
keyToKey intCode pointerOffsetParam =
  memory intCode Vec.! (pointer intCode + pointerOffsetParam)

pw :: IntCodeState -> PointerOffset -> Key
pw =
  keyToKey

pr :: IntCodeState -> PointerOffset -> Value
pr intCode pointerOffsetParam =
  memory intCode Vec.! keyToKey intCode pointerOffsetParam

aParam :: Instruction -> IntCodeState -> Int
aParam instruction intcode =
  case instruction Map.! 'a' of
    0 -> pw intcode pointerOffsetA -- a-p-w
    _ -> error "Instruction is not valid"

bParam :: Instruction -> IntCodeState -> Int
bParam instruction intcode =
  case instruction Map.! 'b' of
    0 -> pr intcode pointerOffsetB -- b-p-r
    _ -> error "Instruction is not valid"

cParam :: Instruction -> IntCodeState -> Int
cParam instruction intcode =
  case instruction Map.! 'c' of
    0 -> pr intcode pointerOffsetC -- c-p-r
    _ -> error "Instruction is not valid"

-- -- 2. Define a function to modify state inside the Monad
-- -- It returns a String confirmation message, and mutates InventoryState
-- addItem :: String -> Double -> State InventoryState String
-- addItem itemName itemWeight = do
--   -- 'get' fetches the current state snapshot
--   currentState <- get

--   let currentWeight = totalWeight currentState
--   let maxWeight = 10.0

--   if currentWeight + itemWeight > maxWeight
--     then return ("Too heavy! Could not add " ++ itemName)
--     else do
--       -- 'put' overwrites the old state with a new state
--       put $
--         InventoryState
--           { items = itemName : items currentState,
--             totalWeight = currentWeight + itemWeight
--           }
--       return ("Successfully added " ++ itemName)

add :: Instruction -> State IntCodeState IntCodeAction
add instruction = do
  currentState <- get
  put $
    IntCodeState
      { pointer = pointer currentState + 4,
        memory =
          memory currentState
            Vec.// [ ( aParam instruction currentState,
                       cParam instruction currentState + bParam instruction currentState
                     )
                   ]
      }
  return Add

multiply :: Instruction -> State IntCodeState IntCodeAction
multiply instruction = do
  currentState <- get
  put $
    IntCodeState
      { pointer = pointer currentState + 4,
        memory =
          memory currentState
            Vec.// [ ( aParam instruction currentState,
                       cParam instruction currentState * bParam instruction currentState
                     )
                   ]
      }
  return Multiply

-- -- 3. Sequence multiple stateful operations inside a do-block
-- gameSession :: State InventoryState [String]
-- gameSession = do
--   msg1 <- addItem "Iron Sword" 4.5
--   msg2 <- addItem "Healing Potion" 1.0
--   msg3 <- addItem "Heavy Gold Chest" 8.0 -- This one should fail due to weight limits
--   return [msg1, msg2, msg3]

runOpCode :: IntCodeState -> IntCodeState
runOpCode intCode =
  case instruction Map.! 'e' of
    1 -> runOpCode (add instruction intCode)
    2 -> runOpCode (multiply instruction intCode)
    9 -> intCode
    _ -> error "Instruction is not valid"
  where
    instruction = makeInstruction (memory intCode Vec.! pointer intCode)

-- -- The state is an Int (the current countdown number).
-- -- The result of the computation is a list of Strings (the emitted outputs).
-- type CountdownState = State Int [String]

-- countdownStep :: CountdownState
-- countdownStep = do
--   n <- get -- Get the current state
--   if n <= 0
--     then do
--       return ["Blast off!"]
--     else do
--       put (n - 1) -- Update state (decrement)
--       let msg = "Counting down: " ++ show n
--       rest <- countdownStep -- Recursively call next step
--       return (msg : rest) -- Cons the current output to the rest

-- main :: IO ()
-- main = do
--   let (outputs, finalState) = runState countdownStep 5
--   print outputs
--   -- mapM_ putStrLn outputs
--   putStrLn $ "Final state: " ++ show finalState
