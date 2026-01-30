module Lib (someFunc) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Case =
  Case
    { description :: String,
      input :: Integer,
      expected :: Bool
    }
