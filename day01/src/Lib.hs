module Lib (gasA, sumGasA) where

import Numeric.Natural (Natural)

gasA :: Natural -> Natural
gasA myModule = div myModule 3 - 2

sumGasA :: [Natural] -> Natural
sumGasA = sum . map gasA
