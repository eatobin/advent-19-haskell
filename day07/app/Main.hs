module Main (main) where

import qualified Data.List.Unique as DLU
import Lib (IntCodeStruct (..), Memory, Output, makeMemory, runOpCode)
import Text.Printf (printf)

type Possibility = [Int]

type Possibilities = [[Int]]

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
    let memoryAsCSVString = "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0"
    let theMemory = makeMemory memoryAsCSVString

    let answer1 = maximum (passes theMemory)

    printf "\nPart A answer = %u. Correct = 65210.\n" answer1

-- let answer2 = output (runOpCode IntCode {input = 5, output = 0, phase = -1, pointer = 0, memory = theMemory, stopped = False, recur = True})

-- printf "Part B answer = %u. Correct = 11981754.\n\n" (answer2 :: Int)
