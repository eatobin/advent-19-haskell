module Main (main) where

import Lib ()

main :: IO ()
main =
  do
    let n = 42 :: Int
    let s = show n
    putStrLn s
