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


-- The top-level function signature
myLength :: [a] -> Int
myLength xs = go xs 0  -- Calls the inner helper 'go'

  where
    -- The inner, tail-recursive helper function
    go :: [a] -> Int -> Int
    go [] n     = n          -- Base case: return the accumulator when the list is empty
    go (_:xs) n = go xs (n + 1) -- Recursive case: increment accumulator and recurse on the tail
