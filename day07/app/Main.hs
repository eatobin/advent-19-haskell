module Main (main) where

import qualified Data.Map.Strict as Map
import Lib (IntCodeStruct (..), makeMemory, runOpCode)
import Text.Printf (printf)

type Key = Int

type Value = Int

type Memory = Map.Map Key Value

type PossibilityFive = [Int]

type Possibilities = [PossibilityFive]

type PassMap = Map.Map Int IntCodeStruct

type NewInput = Int

type PassMapKey = Int

type Output = Int

allPossibilities :: PossibilityFive -> Possibilities
allPossibilities [] = [[]]
allPossibilities (x : xs) = concatMap (interleave x) (allPossibilities xs)
  where
    interleave aa [] = [[aa]]
    interleave bb (y : ys) = (bb : y : ys) : map (y :) (interleave bb ys)

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

grabMyInputFromLastOutput :: PassMapKey -> PassMap -> PassMap
grabMyInputFromLastOutput passMapKey thisPassMap =
  if passMapKey == 1
    then
      updateInputInPassMap passMapKey (output (thisPassMap Map.! 5)) thisPassMap
    else
      updateInputInPassMap passMapKey (output (thisPassMap Map.! pred passMapKey)) thisPassMap

runMyOutputFromMyInput :: PassMapKey -> PassMap -> PassMap
runMyOutputFromMyInput passMapKey thisPassMap =
  Map.insert passMapKey (runOpCode (thisPassMap Map.! passMapKey)) thisPassMap

grabAndRun :: PassMapKey -> PassMap -> PassMap
grabAndRun passMapKey thisPassMap =
  runMyOutputFromMyInput passMapKey (grabMyInputFromPriorOutput passMapKey thisPassMap)

grabAndRun2 :: PassMapKey -> PassMap -> PassMap
grabAndRun2 passMapKey thisPassMap =
  runMyOutputFromMyInput passMapKey (grabMyInputFromLastOutput passMapKey thisPassMap)

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

pass2 :: Memory -> PossibilityFive -> Output
pass2 intcodeMemory [a, b, c, d, e] =
  let go :: PassMapKey -> PassMap -> Output
      go passMapKey thisPassMap =
        if stopped (thisPassMap Map.! 5)
          then
            output (thisPassMap Map.! 5)
          else
            go (succ (mod passMapKey 5)) (grabAndRun2 passMapKey thisPassMap)
   in go
        1
        ( Map.fromList
            [ (1, IntCode {input = 0, output = 0, phase = a, pointer = 0, memory = intcodeMemory, stopped = False, recur = False}),
              (2, IntCode {input = 0, output = 0, phase = b, pointer = 0, memory = intcodeMemory, stopped = False, recur = False}),
              (3, IntCode {input = 0, output = 0, phase = c, pointer = 0, memory = intcodeMemory, stopped = False, recur = False}),
              (4, IntCode {input = 0, output = 0, phase = d, pointer = 0, memory = intcodeMemory, stopped = False, recur = False}),
              (5, IntCode {input = 0, output = 0, phase = e, pointer = 0, memory = intcodeMemory, stopped = False, recur = False})
            ]
        )
pass2 _ _ = error "possibility and/or memory is wrong"

passes :: Memory -> [Output]
passes intcodeMemory =
  map (pass intcodeMemory) (allPossibilities [0, 1, 2, 3, 4])

passes2 :: Memory -> [Output]
passes2 intcodeMemory =
  map (pass2 intcodeMemory) (allPossibilities [5, 6, 7, 8, 9])

main :: IO ()
main =
  do
    let memoryAsCSVString = "3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99"
    let theMemory = makeMemory memoryAsCSVString

    let answerA = maximum (passes theMemory)
    let answerB = maximum (passes2 theMemory)

    printf "\nPart A answer = %u. Correct = 368584.\n" answerA
    printf "Part B answer = %u. Correct = 35993240.\n\n" answerB
