module Main (main) where

import qualified Data.List.Unique as DLU
import qualified Data.Map.Strict as Map
import Lib (IntCodeStruct (..), makeMemory, runOpCode)
import Text.Printf (printf)

type PossibilityFive = [Int]

type Possibilities = [PossibilityFive]

type PassMap = Map.Map Int IntCodeStruct

type NewInput = Int

type PassMapKey = Int

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

updateIntCodeStructInPassMap :: PassMapKey -> IntCodeStruct -> Map.Map PassMapKey IntCodeStruct -> Map.Map PassMapKey IntCodeStruct
updateIntCodeStructInPassMap = Map.insert

updateInputInPassMap :: PassMapKey -> NewInput -> Map.Map PassMapKey IntCodeStruct -> Map.Map PassMapKey IntCodeStruct
updateInputInPassMap passMapKey newInput thisPassMap =
  updateIntCodeStructInPassMap passMapKey newIntCodeStruct thisPassMap
  where
    oldIntCodeStruct = thisPassMap Map.! passMapKey
    newIntCodeStruct = updateInputInIntCodeStruct newInput oldIntCodeStruct

grabMyInputFromPriorOutput :: PassMapKey -> Map.Map PassMapKey IntCodeStruct -> Map.Map PassMapKey IntCodeStruct
grabMyInputFromPriorOutput passMapKey thisPassMap =
  if passMapKey == 1
    then
      updateInputInPassMap passMapKey 0 thisPassMap
    else
      updateInputInPassMap passMapKey (output (thisPassMap Map.! pred passMapKey)) thisPassMap

main :: IO ()
main = do
  -- print (updateIntCodeInPassMap 5 IntCode {input = 11, output = 111, phase = 1, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True} passMap)
  -- print (updateInputInPassMap 1 888 passMap)
  print ""
  print (grabMyInputFromPriorOutput 1 passMap)
  print ""
  print (grabMyInputFromPriorOutput 5 passMap)
  printf "\nPart A answer = %u. Correct = 368584.\n" (42 :: Int)

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
