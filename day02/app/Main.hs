module Main (main) where

import Lib (case1, lookupResult1, lookupResult2, lookupResult3, updatedMap)

main :: IO ()
main =
  do
    print lookupResult1
    print lookupResult2
    print updatedMap
    print lookupResult3
    print case1
