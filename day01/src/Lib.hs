module Lib (someFunc, mySomeFunc, gas, sumGas) where

import Numeric.Natural (Natural)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

mySomeFunc :: Natural -> Natural
mySomeFunc x = x + 1

gas :: Natural -> Natural
gas myModule = div myModule 3 - 2

sumGas :: [Natural] -> Natural
sumGas = sum . map gas
