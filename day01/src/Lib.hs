module Lib (mySomeFunc, someFunc, gas) where

import Numeric.Natural (Natural)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

mySomeFunc :: Natural -> Natural
mySomeFunc x = x + 1

gas :: Natural -> Natural
gas myModule = div myModule 3 - 2
