module Lib (gas, sumGasA, gasPlus, sumGasB) where

gas :: Int -> Int
gas myModule = div myModule 3 - 2

-- >>> gas 6
-- 0
-- >>> gas 9
-- 1
-- >>> gas 1969
-- 654
-- >>> gas 100756
-- 33583

sumGasA :: [Int] -> Int
sumGasA = sum . map gas

gasPlus :: Int -> Int
gasPlus outerModule =
  loop outerModule 0
  where
    loop :: Int -> Int -> Int
    loop innerModule acc
      | newGas <= 0 = acc
      | otherwise = loop newGas (acc + newGas)
      where
        newGas = gas innerModule

-- >>> gasPlus 100756
-- 50346

sumGasB :: [Int] -> Int
sumGasB = sum . map gasPlus
