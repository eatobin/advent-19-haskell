module Lib (gas, sumGas) where

import Numeric.Natural (Natural)

gas :: Natural -> Natural
gas myModule = div myModule 3 - 2

sumGas :: [Natural] -> Natural
sumGas = sum . map gas
