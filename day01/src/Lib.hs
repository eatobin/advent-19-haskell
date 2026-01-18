module Lib (someFunc, gas) where

someFunc :: Int
someFunc = 101

gas :: Int -> Int
gas myModule = (-) (div myModule 3) 2
