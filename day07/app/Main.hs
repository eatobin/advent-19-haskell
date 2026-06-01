{-# LANGUAGE TemplateHaskell #-}

import Lens.Micro.Platform (makeLenses, (%~), (&), (^.))

newtype Address = Address {_city :: String} deriving (Show)

data User = User {_name :: String, _address :: Address} deriving (Show)

-- Automatically generates _city and _name lenses
makeLenses ''Address
makeLenses ''User

main :: IO ()
main = do
  let user = User "Alice" (Address "Phoenix")

  -- 1. Get a nested field
  print $ user ^. address . city -- Output: "Phoenix"

  -- 2. Modify a field
  print $ user & name %~ (++ " Smith") -- Output: User "Alice Smith" ...

-- Google haskell microlens platform tutorial
