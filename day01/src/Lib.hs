module Lib (gasA, gasB, sumGasA, sumGasB) where

import Numeric.Natural (Natural)

gasA :: Natural -> Natural
gasA myModule = div myModule 3 - 2

sumGasA :: [Natural] -> Natural
sumGasA = sum . map gasA

gasB :: Natural -> Int
gasB myModule = div (fromIntegral myModule) 3 - 2

sumGasB :: [Natural] -> Int
sumGasB = sum . map gasB
