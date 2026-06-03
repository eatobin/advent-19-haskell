{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Map.Strict as Map
import Lens.Micro.Platform (makeLenses, (%~), (&), (.~), (^.))
import Lib (IntCodeStruct (..), Memory)

-- data IntCode = IntCode {_inputIC :: Int, _outputIC :: Int, phase :: Int, pointer :: Int, memory :: Memory, stopped :: Bool, recur :: Bool} deriving (Show)
type PassMap = Map.Map Int IntCodeStruct

passMap :: PassMap
passMap =
  Map.fromList
    [ (1, IntCode {_input = 11, _output = 0, phase = 1, pointer = 0, memory = Map.fromList [(1, 1)], stopped = False, recur = True}),
      (2, IntCode {_input = 22, _output = 0, phase = 2, pointer = 0, memory = Map.fromList [(2, 2)], stopped = False, recur = True}),
      (3, IntCode {_input = 0, _output = 0, phase = 3, pointer = 0, memory = Map.fromList [(3, 3)], stopped = False, recur = True}),
      (4, IntCode {_input = 0, _output = 0, phase = 4, pointer = 0, memory = Map.fromList [(4, 4)], stopped = False, recur = True}),
      (5, IntCode {_input = 55, _output = 555, phase = 5, pointer = 0, memory = Map.fromList [(5, 5)], stopped = False, recur = True})
    ]

makeLenses ''IntCodeStruct

main :: IO ()
main = do
  -- let tinyA = IC 111 7777 0 0 (Map.fromList [(1, 1)]) True True
  -- let tinyB = IC 222 8888 0 0 (Map.fromList [(2, 2)]) True True
  -- let tinyC = IC 333 9999 0 0 (Map.fromList [(3, 3)]) True True
  -- let big3 =
  --       Map.fromList
  --         [ (1, IC 111 7777 0 0 (Map.fromList [(1, 1)]) True True),
  --           (2, IC 222 8888 0 0 (Map.fromList [(2, 2)]) True True),
  --           (3, IC 333 9999 0 0 (Map.fromList [(3, 3)]) True True)
  --         ] ::
  --         Map.Map Int IC

  print $ passMap Map.! 1 ^. input
  print $ passMap Map.! 5 ^. output

  print $ passMap Map.! 1 & input %~ succ
  print $ passMap Map.! 3 & output .~ 42
  print $ passMap Map.! 1 & input .~ (passMap Map.! 5 ^. output)
