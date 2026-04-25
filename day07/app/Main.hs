module Main (main) where

import qualified Data.List.Unique as DLU
import Lib (IntCodeStruct (..), Memory, Output, Phase, makeMemory, runOpCode)
import Text.Printf (printf)

type Possibility = [Phase]

type Possibilities = [Possibility]

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

pass :: Memory -> Possibility -> Output
pass intcodeMemory [a, b, c, d, e] =
  let opA = IntCode {input = 0, output = 0, phase = a, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
      opB = IntCode {input = output (runOpCode opA), output = 0, phase = b, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
      opC = IntCode {input = output (runOpCode opB), output = 0, phase = c, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
      opD = IntCode {input = output (runOpCode opC), output = 0, phase = d, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
      opE = IntCode {input = output (runOpCode opD), output = 0, phase = e, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
   in output (runOpCode opE)
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

-- let answer2 = output (runOpCode IntCode {input = 5, output = 0, phase = -1, pointer = 0, memory = theMemory, stopped = False, recur = True})

-- printf "Part B answer = %u. Correct = 11981754.\n\n" (answer2 :: Int)
