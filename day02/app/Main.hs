module Main (main) where

import Control.Monad.Trans.State (execState)
import qualified Data.Vector as Vec
import Lib (IntCode (..), makeMemory, runOpCode, updatedMemory)
import Text.Printf (printf)

main :: IO ()
main =
  do
    let memoryAsCSVString = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,10,1,19,2,9,19,23,2,13,23,27,1,6,27,31,2,6,31,35,2,13,35,39,1,39,10,43,2,43,13,47,1,9,47,51,1,51,13,55,1,55,13,59,2,59,13,63,1,63,6,67,2,6,67,71,1,5,71,75,2,6,75,79,1,5,79,83,2,83,6,87,1,5,87,91,1,6,91,95,2,95,6,99,1,5,99,103,1,6,103,107,1,107,2,111,1,111,5,0,99,2,14,0,0"
    let firstMemory = makeMemory memoryAsCSVString
    let initialState = IntCode {pointer = 0, memory = updatedMemory 12 2 firstMemory, actions = []}

    let finalStateA = execState runOpCode initialState
    let answer1 = memory finalStateA Vec.! 0

    printf "\nPart A answer = %u. Correct = 2890696.\n" (answer1 :: Int)
    print $ reverse (actions finalStateA)

    let answer2 =
          head
            [ (100 * noun) + verb
              | noun <- [0 .. (99 :: Int)],
                verb <- [0 .. (99 :: Int)],
                let candidate = memory (execState runOpCode IntCode {pointer = 0, memory = updatedMemory noun verb firstMemory, actions = []}) Vec.! 0,
                candidate == 19690720
            ]

    printf "\nPart B answer = %u. Correct = 8226.\n\n" (answer2 :: Int)
