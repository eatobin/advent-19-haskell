{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Map.Strict as Map
import Lens.Micro.Platform (makeLenses, (%~), (&), (.~), (^.))
import Lib (Memory)

data IC = IC {_inputIC :: Int, _outputIC :: Int, phase :: Int, pointer :: Int, memory :: Memory, stopped :: Bool, recur :: Bool} deriving (Show)

makeLenses ''IC

main :: IO ()
main = do
  -- let tinyA = IC 111 7777 0 0 (Map.fromList [(1, 1)]) True True
  -- let tinyB = IC 222 8888 0 0 (Map.fromList [(2, 2)]) True True
  -- let tinyC = IC 333 9999 0 0 (Map.fromList [(3, 3)]) True True
  let big3 =
        Map.fromList
          [ (1, IC 111 7777 0 0 (Map.fromList [(1, 1)]) True True),
            (2, IC 222 8888 0 0 (Map.fromList [(2, 2)]) True True),
            (3, IC 333 9999 0 0 (Map.fromList [(3, 3)]) True True)
          ] ::
          Map.Map Int IC

  print $ big3 Map.! 1 ^. inputIC
  print $ big3 Map.! 2 ^. outputIC
  print $ big3 Map.! 3 ^. inputIC

  print $ big3 Map.! 1 & inputIC %~ succ
  print $ big3 Map.! 3 & outputIC .~ 42
  print $ big3 Map.! 1 & inputIC .~ (big3 Map.! 3 ^. outputIC)
