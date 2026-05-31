module Main (main) where

import qualified Data.List.Unique as DLU
import qualified Data.Map.Strict as Map
import Foreign (new)
import Lib (IntCodeStruct (..), Memory, Output, Phase, makeMemory, runOpCode)
import Text.Printf (printf)

type Possibility = [Phase]

type PassMap = Map.Map Int IntCodeStruct

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

passMap :: PassMap
passMap =
  Map.fromList
    [ (1, IntCode {input = 0, output = 0, phase = 1, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True}),
      (2, IntCode {input = 0, output = 0, phase = 2, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True}),
      (3, IntCode {input = 0, output = 0, phase = 1, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True}),
      (4, IntCode {input = 0, output = 0, phase = 1, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True}),
      (5, IntCode {input = 0, output = 0, phase = 1, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True})
    ]

tinyA :: PassMap
tinyA = Map.fromList [(1, IntCode {input = 0, output = 0, phase = 1, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True})]

tinyB :: PassMap
tinyB = Map.fromList [(2, IntCode {input = 0, output = 0, phase = 1, pointer = 0, memory = Map.fromList [(11, 11)], stopped = False, recur = True})]

tiny99 :: PassMap
tiny99 = Map.fromList [(99, IntCode {input = 0, output = 0, phase = 1, pointer = 0, memory = Map.fromList [(99, 9)], stopped = False, recur = True})]

grabMyInputFromPriorOutput :: Int -> PassMap -> PassMap
grabMyInputFromPriorOutput myIndex thisPassMap =
  let newMap = thisPassMap
   in if myIndex == 1
        then
          newMap
        else
          tiny99

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

-- (defn grab-my-input-from-prior-output [my-index this-pass-map]
--   (if (= my-index 1)
--     (assoc-in this-pass-map [my-index :input] 0)
--     (assoc-in this-pass-map [my-index :input] (last (get-in this-pass-map [(dec my-index) :output])))))

-- (defn run-my-output-from-my-input [my-index this-pass-map]
--   (assoc
--    this-pass-map
--    my-index
--    (ic/op-code (get this-pass-map my-index))))

-- (defn grab-and-run [my-index this-pass-map]
--   (->>
--    this-pass-map
--    (grab-my-input-from-prior-output my-index)
--    (run-my-output-from-my-input my-index)))

-- (defn pass [[a b c d e]]
--   (loop [my-index      1
--          this-pass-map {1 {:input 0 :output [] :phase a :pointer 0 :relative-base 0 :memory memory :stopped? false :recur? true}
--                         2 {:input 0 :output [] :phase b :pointer 0 :relative-base 0 :memory memory :stopped? false :recur? true}
--                         3 {:input 0 :output [] :phase c :pointer 0 :relative-base 0 :memory memory :stopped? false :recur? true}
--                         4 {:input 0 :output [] :phase d :pointer 0 :relative-base 0 :memory memory :stopped? false :recur? true}
--                         5 {:input 0 :output [] :phase e :pointer 0 :relative-base 0 :memory memory :stopped? false :recur? true}}]
--     (if (get-in this-pass-map [5 :stopped?])
--       (first (get-in this-pass-map [5 :output]))
--       (recur
--        (inc (mod my-index 5))
--        (grab-and-run my-index this-pass-map)))))

-- (defn passes []
--   (map #(pass %) possibles))

-- (def answer (apply max (passes)))

-- (comment
--   answer
--   368584
--   :rcf)
