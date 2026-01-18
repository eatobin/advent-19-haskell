module Lib (someFunc, gas) where

import Numeric.Natural (Natural)

someFunc :: Natural -> Natural
someFunc x = (+) x 1

gas :: Natural -> Natural
gas myModule = (-) (div myModule 3) 2
