module Main (main) where

import qualified Data.List.Split as DLS
import qualified Data.List.Unique as DLU
import qualified Data.Map.Strict as Map
import Lib (IntCodeStruct (..), runOpCode)
import Text.Printf (printf)

type MemoryAsCSVString = [Char]

type Key = Int

type Value = Int

type Memory = Map.Map Key Value

type PossibilityFive = [Int]

type Possibilities = [PossibilityFive]

type PassMap = Map.Map Int IntCodeStruct

type NewInput = Int

type PassMapKey = Int

type Output = Int

makeMemory :: MemoryAsCSVString -> Memory
makeMemory memoryAsCSVStringParam =
  let memoryAsKVTupleList = zip [0 ..] (map read (DLS.splitOn "," memoryAsCSVStringParam))
   in Map.fromList memoryAsKVTupleList

possibilities :: Possibilities
possibilities =
  [ [a, b, c, d, e]
    | a <- [0 .. 4 :: Int],
      b <- [0 .. 4 :: Int],
      c <- [0 .. 4 :: Int],
      d <- [0 .. 4 :: Int],
      e <- [0 .. 4 :: Int],
      DLU.allUnique [a, b, c, d, e]
  ]

updateInputInIntCodeStruct :: NewInput -> IntCodeStruct -> IntCodeStruct
updateInputInIntCodeStruct newInput intCodeStruct =
  intCodeStruct {input = newInput}

updateIntCodeStructInPassMap :: PassMapKey -> IntCodeStruct -> PassMap -> PassMap
updateIntCodeStructInPassMap = Map.insert

updateInputInPassMap :: PassMapKey -> NewInput -> PassMap -> PassMap
updateInputInPassMap passMapKey newInput thisPassMap =
  updateIntCodeStructInPassMap passMapKey newIntCodeStruct thisPassMap
  where
    oldIntCodeStruct = thisPassMap Map.! passMapKey
    newIntCodeStruct = updateInputInIntCodeStruct newInput oldIntCodeStruct

grabMyInputFromPriorOutput :: PassMapKey -> PassMap -> PassMap
grabMyInputFromPriorOutput passMapKey thisPassMap =
  if passMapKey == 1
    then
      updateInputInPassMap passMapKey 0 thisPassMap
    else
      updateInputInPassMap passMapKey (output (thisPassMap Map.! pred passMapKey)) thisPassMap

runMyOutputFromMyInput :: PassMapKey -> PassMap -> PassMap
runMyOutputFromMyInput passMapKey thisPassMap =
  Map.insert passMapKey (runOpCode (thisPassMap Map.! passMapKey)) thisPassMap

grabAndRun :: PassMapKey -> PassMap -> PassMap
grabAndRun passMapKey thisPassMap =
  runMyOutputFromMyInput passMapKey (grabMyInputFromPriorOutput passMapKey thisPassMap)

pass :: Memory -> PossibilityFive -> Output
pass intcodeMemory [a, b, c, d, e] =
  let go :: PassMapKey -> PassMap -> Output
      go passMapKey thisPassMap =
        if stopped (thisPassMap Map.! 5)
          then
            output (thisPassMap Map.! 5)
          else
            go (succ (mod passMapKey 5)) (grabAndRun passMapKey thisPassMap)
   in go
        1
        ( Map.fromList
            [ (1, IntCode {input = 0, output = 0, phase = a, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
              (2, IntCode {input = 0, output = 0, phase = b, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
              (3, IntCode {input = 0, output = 0, phase = c, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
              (4, IntCode {input = 0, output = 0, phase = d, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
              (5, IntCode {input = 0, output = 0, phase = e, pointer = 0, memory = intcodeMemory, stopped = False, recur = True})
            ]
        )
pass _ _ = error "possibility and/or memory is wrong"

passes :: Memory -> [Output]
passes intcodeMemory =
  map (pass intcodeMemory) possibilities

main :: IO ()
main =
  do
    let memoryAsCSVString = "3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99"
    let theMemory = makeMemory memoryAsCSVString

    let answer1 = maximum (passes theMemory)

    printf "\nPart A answer = %u. Correct = 368584.\n" answer1
