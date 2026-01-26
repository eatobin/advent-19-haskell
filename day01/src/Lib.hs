module Lib (gasA, sumGasA) where

import Numeric.Natural (Natural)

gasA :: Int -> Int
gasA myModule = div myModule 3 - 2

sumGasA :: [Int] -> Int
sumGasA = sum . map gasA

-- gasB :: Natural -> Int
-- gasB myModule = div (fromIntegral myModule) 3 - 2

-- sumGasB :: [Natural] -> Int
-- sumGasB = sum . map gasB

-- The top-level function signature
myLength :: [a] -> Int
myLength xs = go xs 0 -- Calls the inner helper 'go'
  where
    -- The inner, tail-recursive helper function
    go :: [a] -> Int -> Int
    go [] n = n -- Base case: return the accumulator when the list is empty
    go (_ : ys) n = go ys (n + 1) -- Recursive case: increment accumulator and recurse on the tail

holeScore :: Int -> Int -> String
holeScore strokes par
 | score < 0 = show (abs score) ++ " under par"
 | score == 0 = "level par"
 | otherwise = show(score) ++ " over par"
 where score = strokes-par

-- let gasPlus (outerModule: int) : int =
--     let rec loop (innerModule: int) (acc: int) : int =
--         let newGas = gas innerModule

--         match newGas with
--         | newGas when newGas <= 0 -> acc
--         | _ -> loop newGas (acc + newGas)

--     loop outerModule 0


gasPlus :: Int -> Int
gasPlus outerModule = loop outerModule 0
    loop innerModule acc
    | newGas <= 0 = acc
    | otherwise = loop newGas (acc + newGas)
    where newGas = gasA innerModule
