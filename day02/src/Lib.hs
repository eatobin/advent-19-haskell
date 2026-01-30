module Lib (someFunc) where

someFunc :: IO ()
someFunc = print case1

data Case =
  Case
    { description :: String,
      input :: Integer,
      expected :: Bool
    }
  deriving Show

case1 :: Case
case1 = Case { description = "456 Oak Ave", input = 42, expected = True }
