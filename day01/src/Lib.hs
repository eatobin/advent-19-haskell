module Lib (gasA, gasB, sumGasA, sumGasB) where

import Numeric.Natural (Natural)

gasA :: Natural -> Natural
gasA myModule = div myModule 3 - 2

sumGasA :: [Natural] -> Natural
sumGasA = sum . map gasA

gasB :: Int -> Int
gasB myModule = div myModule 3 - 2

sumGasB :: [Int] -> Int
sumGasB = sum . map gasB
