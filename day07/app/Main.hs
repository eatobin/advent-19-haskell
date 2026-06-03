{-# LANGUAGE TemplateHaskell #-}

import qualified Data.Map.Strict as Map
import Lens.Micro.Platform (makeLenses, (%~), (&), (.~), (^.))
import Lib (Memory)

data Address = Address {_city :: String, _state :: String} deriving (Show)

data User = User {_name :: String, _address :: Address} deriving (Show)

data IC = IC {_inputIC :: Int, _outputIC :: Int, phase :: Int, pointer :: Int, memory :: Memory, stopped :: Bool, recur :: Bool} deriving (Show)

-- data MyPass = MyPass {_pass :: Int, _ic :: IC} deriving (Show)

-- Automatically generates _city and _name lenses
makeLenses ''Address
makeLenses 'User
makeLenses ''IC

-- makeLenses ''MyPass

main :: IO ()
main = do
  let user = User "Alice" (Address "Phoenix" "AZ")
  let tinyA = IC 111 7777 0 0 (Map.fromList [(1, 1)]) True True
  let tinyB = IC 222 8888 0 0 (Map.fromList [(2, 2)]) True True
  let tinyC = IC 333 9999 0 0 (Map.fromList [(3, 3)]) True True
  -- let myAddress = Address "Nome" "AL"
  -- let big = Map.fromList [(1, 11111)] :: Map.Map Int Int
  let big3 = Map.fromList [(1, tinyA), (2, tinyB), (3, tinyC)] :: Map.Map Int IC

  -- 1. Get a nested field
  print $ user ^. address . city
  print $ user ^. address . state
  print $ user ^. name
  -- print $ tinyA ^. ic . inputIC
  -- print $ tinyA ^. ic . outputIC
  -- print $ tinyA ^. pass
  -- print $ myAddress ^. city
  -- print $ myAddress ^. state
  print $ big3 Map.! 1 ^. inputIC
  print $ big3 Map.! 2 ^. outputIC
  print $ big3 Map.! 3 ^. inputIC

  -- 2. Modify a field
  print $ user & name %~ (++ " Smith") -- Output: User "Alice Smith" ...
  print $ big3 Map.! 1 & inputIC %~ succ
  print $ big3 Map.! 3 & outputIC .~ 42
  print $ big3 Map.! 1 & inputIC .~ (big3 Map.! 3 ^. outputIC)

-- Google haskell microlens platform tutorial
-- print $ user ^. address . state
