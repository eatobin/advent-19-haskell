{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Main (main) where

import qualified Data.List.Split as DLS
import qualified Data.List.Unique as DLU
import qualified Data.Map.Strict as Map
import Lib (IntCodeStruct (..), makeInstruction, runOpCode)
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

passMap :: PassMap
passMap =
  Map.fromList
    [ (1, IntCode {input = 11, output = 111, phase = 1, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True}),
      (2, IntCode {input = 22, output = 222, phase = 2, pointer = 0, memory = Map.fromList [(2, 2)], stopped = False, recur = True}),
      (3, IntCode {input = 33, output = 333, phase = 3, pointer = 0, memory = Map.fromList [(3, 3)], stopped = False, recur = True}),
      (4, IntCode {input = 44, output = 444, phase = 4, pointer = 0, memory = Map.fromList [(4, 4)], stopped = False, recur = True}),
      (5, IntCode {input = 55, output = 555, phase = 5, pointer = 0, memory = Map.fromList [(5, 5)], stopped = False, recur = True})
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

go :: PassMapKey -> PassMap -> Output
go passMapKey thisPassMap =
  if stopped (thisPassMap Map.! 5)
    then
      output (thisPassMap Map.! 5)
    else
      go (succ (mod passMapKey 5)) (grabAndRun passMapKey thisPassMap)

-- go _ _ = error "some argument is wrong"


pass :: Memory -> PossibilityFive -> Output
pass intcodeMemory [a, b, c, d, e] =
  let passMapKey = 1
      thisPassMap =
       Map.fromList
       [ (1, IntCode {input = 0, output = 0, phase = a, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
         (2, IntCode {input = 0, output = 0, phase = b, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
         (3, IntCode {input = 0, output = 0, phase = c, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
         (4, IntCode {input = 0, output = 0, phase = d, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
         (5, IntCode {input = 0, output = 0, phase = e, pointer = 0, memory = intcodeMemory, stopped = False, recur = True})
       ]
      go passMapKey thisPassMap =
       if stopped (thisPassMap Map.! 5)
        then
          output (thisPassMap Map.! 5)
        else
         go (succ (mod passMapKey 5)) (grabAndRun passMapKey thisPassMap) 
   in


-- pass :: Memory -> PossibilityFive -> Output
-- pass intcodeMemory [a, b, c, d, e] =
--   let opA = IntCode {input = 0, output = 0, phase = a, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
--       opB = IntCode {input = output (runOpCode opA), output = 0, phase = b, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
--       opC = IntCode {input = output (runOpCode opB), output = 0, phase = c, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
--       opD = IntCode {input = output (runOpCode opC), output = 0, phase = d, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
--       opE = IntCode {input = output (runOpCode opD), output = 0, phase = e, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}
--    in output (runOpCode opE)
-- pass _ _ = error "possibility and/or memory is wrong"

-- pass :: Memory -> Possibility -> Output
-- pass intcodeMemory [a, b, c, d, e] =
--   let go passMapKey thisPassMap = if stopped (thisPassMap Map.! 5) then output (thisPassMap Map.! 5) else 0
--    in go 6

-- let go passMapKey thisPassMap =
--   in go 1 thisPassMap
-- passMapKey = 1
-- thisPassMap =
--   Map.fromList
--   [ (1, IntCode {input = 0, output = 0, phase = a, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
--     (2, IntCode {input = 0, output = 0, phase = b, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
--     (3, IntCode {input = 0, output = 0, phase = c, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
--     (4, IntCode {input = 0, output = 0, phase = d, pointer = 0, memory = intcodeMemory, stopped = False, recur = True}),
--     (5, IntCode {input = 0, output = 0, phase = e, pointer = 0, memory = intcodeMemory, stopped = False, recur = True})
--   ]

-- sumListLet :: [Int] -> Int
-- sumListLet xs =
--   let go [] acc = acc
--       go (y : ys) acc = go ys (acc + y)
--    in go xs 0

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

-- passes :: Memory -> [Output]
-- passes intcodeMemory =
--   map (pass intcodeMemory) possibilities

-- main :: IO ()
-- main =
--   do
--     let memoryAsCSVString = "3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99"
--     let theMemory = makeMemory memoryAsCSVString

--     let answer1 = maximum (passes theMemory)

--     printf "\nPart A answer = %u. Correct = 368584.\n" answer1

main :: IO ()
main = do
  -- print (updateIntCodeInPassMap 5 IntCode {input = 11, output = 111, phase = 1, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True} passMap)
  -- print (updateInputInPassMap 1 888 passMap)
  print ""
  -- print (grabMyInputFromPriorOutput 1 passMap)
  print ""
  -- print (grabMyInputFromPriorOutput 5 passMap)
  print (runMyOutputFromMyInput 1 passMap)
  print (runMyOutputFromMyInput 5 passMap)

-- printf "\nPart A answer = %u. Correct = 368584.\n" (42 :: Int)

--   let innerMap = M.fromList [("jan", 1), ("feb", 2)] :: M.Map String Int
--   let outerMap = M.fromList [("mine", innerMap), ("yours", innerMap)]
--   print outerMap
--   let newmap = updateNestedValue "yours" "feb" 9 outerMap
--   print newmap

-- -- adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
-- -- Update a value at a specific key with the result of the provided function.
-- --   When the key is not a member of the map, the original map is returned.

-- -- adjust ("new " ++) 5 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "new a")]
-- -- adjust ("new " ++) 7 (fromList [(5,"a"), (3,"b")]) == fromList [(3, "b"), (5, "a")]
-- -- adjust ("new " ++) 7 empty                         == empty
