module Lib (IntCodeStruct (..), makeInstruction, makeMemory, pw, pr, aParam, bParam, cParam, add, multiply, runOpCode) where

-- Map.Map Char Int:
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
import qualified Data.List.Split as DLS
import qualified Data.Map.Strict as Map

-- type Map.Map Char Int = Map.Map Char Int

-- type Pointer = Int

-- type Key = Int

-- type Value = Int

-- type KeyOrValue = Int

-- type Memory = Map.Map Int Int

type MemoryAsCSVString = [Char]

-- type Int = Int

-- type Input = Int

-- type Output = Int

-- type Phase = Int

-- type Stopped = Bool

-- type Recur = Bool

data IntCodeStruct
  = IntCode
  { input :: Int,
    output :: Int,
    phase :: Int,
    pointer :: Int,
    memory :: Map.Map Int Int,
    stopped :: Bool,
    recur :: Bool
  }
  deriving (Eq, Show)

pointerOffsetC :: Int
pointerOffsetC = 1

pointerOffsetB :: Int
pointerOffsetB = 2

pointerOffsetA :: Int
pointerOffsetA = 3

makeInstruction :: Int -> Map.Map Char Int
makeInstruction op =
  Map.fromList instructionAsKVTupleList
  where
    keys = ['a', 'b', 'c', 'd', 'e']
    opAsString = show op
    paddedOp = replicate (5 - length opAsString) '0' ++ opAsString
    values = map DC.digitToInt paddedOp
    instructionAsKVTupleList = zip keys values

makeMemory :: MemoryAsCSVString -> Map.Map Int Int
makeMemory memoryAsCSVStringParam =
  let memoryAsKVTupleList = zip [0 ..] (map read (DLS.splitOn "," memoryAsCSVStringParam))
   in Map.fromList memoryAsKVTupleList

lookupABC :: IntCodeStruct -> Int -> Maybe Int
lookupABC intcode pointerOffsetParam =
  Map.lookup
    (pointer intcode + pointerOffsetParam)
    (memory intcode)

readABC :: IntCodeStruct -> Int -> Int
readABC intcode pointerOffsetParam =
  case lookupABC intcode pointerOffsetParam of
    Just value -> value
    Nothing -> error "Key is not valid"

pw :: IntCodeStruct -> Int -> Int
pw =
  readABC

pr :: IntCodeStruct -> Int -> Int
pr intcode pointerOffsetParam =
  memory intcode Map.! readABC intcode pointerOffsetParam

ir :: IntCodeStruct -> Int -> Int
ir =
  readABC

aParam :: Map.Map Char Int -> IntCodeStruct -> Int
aParam instruction intcode =
  case instruction Map.! 'a' of
    0 -> pw intcode pointerOffsetA -- a-p-w
    _ -> error "Map.Map Char Int is not valid"

bParam :: Map.Map Char Int -> IntCodeStruct -> Int
bParam instruction intcode =
  case instruction Map.! 'b' of
    0 -> pr intcode pointerOffsetB -- b-p-r
    1 -> ir intcode pointerOffsetB -- b-i-r
    _ -> error "Map.Map Char Int is not valid"

cParam :: Map.Map Char Int -> IntCodeStruct -> Int
cParam instruction intcode =
  if instruction Map.! 'e' == 3
    then case instruction Map.! 'c' of
      0 -> pw intcode pointerOffsetC -- c-p-w
      _ -> error "Map.Map Char Int is not valid"
    else case instruction Map.! 'c' of
      0 -> pr intcode pointerOffsetC -- c-p-r
      1 -> ir intcode pointerOffsetC -- c-i-r
      _ -> error "Map.Map Char Int is not valid"

add :: Map.Map Char Int -> IntCodeStruct -> IntCodeStruct
add instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      phase = phase intcode,
      pointer = pointer intcode + 4,
      memory =
        Map.insert
          (aParam instruction intcode)
          (cParam instruction intcode + bParam instruction intcode)
          (memory intcode),
      stopped = stopped intcode,
      recur = recur intcode
    }

multiply :: Map.Map Char Int -> IntCodeStruct -> IntCodeStruct
multiply instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      phase = phase intcode,
      pointer = pointer intcode + 4,
      memory =
        Map.insert
          (aParam instruction intcode)
          (cParam instruction intcode * bParam instruction intcode)
          (memory intcode),
      stopped = stopped intcode,
      recur = recur intcode
    }

takeInput :: Map.Map Char Int -> IntCodeStruct -> IntCodeStruct
takeInput instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      phase = phase intcode,
      pointer = pointer intcode + 2,
      memory =
        if phase intcode == (-1)
          then
            Map.insert
              (cParam instruction intcode)
              (input intcode)
              (memory intcode)
          else
            if pointer intcode == 0
              then
                Map.insert
                  (cParam instruction intcode)
                  (phase intcode)
                  (memory intcode)
              else
                Map.insert
                  (cParam instruction intcode)
                  (input intcode)
                  (memory intcode),
      stopped = stopped intcode,
      recur = recur intcode
    }

giveOutput :: Map.Map Char Int -> IntCodeStruct -> IntCodeStruct
giveOutput instruction intcode =
  IntCode
    { input = input intcode,
      output = cParam instruction intcode,
      phase = phase intcode,
      pointer = pointer intcode + 2,
      memory = memory intcode,
      stopped = stopped intcode,
      recur = recur intcode
    }

jumpIfTrue :: Map.Map Char Int -> IntCodeStruct -> IntCodeStruct
jumpIfTrue instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      phase = phase intcode,
      pointer =
        if cParam instruction intcode /= 0
          then bParam instruction intcode
          else pointer intcode + 3,
      memory = memory intcode,
      stopped = stopped intcode,
      recur = recur intcode
    }

jumpIfFalse :: Map.Map Char Int -> IntCodeStruct -> IntCodeStruct
jumpIfFalse instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      phase = phase intcode,
      pointer =
        if cParam instruction intcode == 0
          then bParam instruction intcode
          else pointer intcode + 3,
      memory = memory intcode,
      stopped = stopped intcode,
      recur = recur intcode
    }

lessThan :: Map.Map Char Int -> IntCodeStruct -> IntCodeStruct
lessThan instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      phase = phase intcode,
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
              (memory intcode),
      stopped = stopped intcode,
      recur = recur intcode
    }

equals :: Map.Map Char Int -> IntCodeStruct -> IntCodeStruct
equals instruction intcode =
  IntCode
    { input = input intcode,
      output = output intcode,
      phase = phase intcode,
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
              (memory intcode),
      stopped = stopped intcode,
      recur = recur intcode
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
    _ -> error "Map.Map Char Int is not valid"
  where
    instruction = makeInstruction (memory intcode Map.! pointer intcode)

-- (defn op-code [{:keys [input output phase pointer relative-base memory stopped? recur?]}]
--   (if stopped?
--     {:input input :output output :phase phase :pointer pointer :relative-base relative-base :memory memory :stopped? stopped? :recur? recur?}
--     (let [instruction (pad-5 (memory pointer))]
--       (case (instruction :e)
--         1 (recur

-- 4 (if recur?
--     (recur
--      {:input         input
--       :output        (conj output (c-param {:instruction instruction :pointer pointer :memory memory :relative-base relative-base}))
--       :phase         phase
--       :pointer       (+ 2 pointer)
--       :relative-base relative-base
--       :memory        memory
--       :stopped?      stopped?
--       :recur?        recur?})
--     {:input         input
--      :output        (conj output (c-param {:instruction instruction :pointer pointer :memory memory :relative-base relative-base}))
--      :phase         phase
--      :pointer       (+ 2 pointer)
--      :relative-base relative-base
--      :memory        memory
--      :stopped?      stopped?
--      :recur?        recur?})

-- 9 (if (= (instruction :d) 9)
--     (recur
--      {:input         input
--       :output        output
--       :phase         phase
--       :pointer       pointer
--       :relative-base relative-base
--       :memory        memory
--       :stopped?      true
--       :recur?        recur?})
--     (recur
--      {:input         input
--       :output        output
--       :phase         phase
--       :pointer       (+ 2 pointer)
--       :relative-base (+ (c-param {:instruction instruction :pointer pointer :memory memory :relative-base relative-base}) relative-base)
--       :memory        memory
--       :stopped?      stopped?
--       :recur?        recur?}))
