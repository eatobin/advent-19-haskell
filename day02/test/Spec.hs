import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map as Map
import Lib (IntCodeStruct (..), aParam, bParam, cParam, makeInstruction, makeMemory, pr, pw)
import Test.Hspec (anyErrorCall, describe, hspec, it, shouldBe, shouldThrow)

main :: IO ()
main = hspec $ do
  describe "\nJust test if tests work" $ do
    it "a test all by itself" $ do
      (78 :: Int) `shouldBe` (78 :: Int)

  describe "\nInstruction Tests" $ do
    it "make an Instruction 1" $ do
      makeInstruction 6 `shouldBe` Map.fromList [('a', 0), ('b', 0), ('c', 0), ('d', 0), ('e', 6 :: Int)]
    it "make an Instruction 3" $ do
      makeInstruction 456 `shouldBe` Map.fromList [('a', 0), ('b', 0), ('c', 4), ('d', 5), ('e', 6 :: Int)]
    it "make an Instruction 5" $ do
      makeInstruction 23456 `shouldBe` Map.fromList [('a', 2), ('b', 3), ('c', 4), ('d', 5), ('e', 6 :: Int)]

  describe "\nIntCodeStruct Tests" $ do
    let memoryAsCSVString = "10,11,1"
    let thisMemory = IntMap.fromList [(0, 10), (1, 11), (2, 1 :: Int)]
    let intCode = IntCode {pointer = 0, memory = thisMemory}

    it "make a Memory" $ do
      makeMemory memoryAsCSVString `shouldBe` thisMemory
    it "lookup a valid Memory index - pw" $ do
      pw intCode 2 `shouldBe` 1
    it "lookup a valid Memory index - pr" $ do
      pr intCode 2 `shouldBe` 11
    it "lookup an invalid Memory index" $ do
      evaluate (pw intCode 33) `shouldThrow` anyErrorCall

  describe "\nxParam Tests" $ do
    let instruction = Map.fromList [('a', 0), ('b', 0), ('c', 0), ('d', 0), ('e', 6 :: Int)]
    let thisMemoryX = IntMap.fromList [(0, 0), (1, 1), (2, 2), (3, 3 :: Int)]
    let intCodeX = IntCode {pointer = 0, memory = thisMemoryX}

    it "lookup a valid aParam" $ do
      aParam instruction intCodeX `shouldBe` 3
    it "lookup a valid bParam" $ do
      bParam instruction intCodeX `shouldBe` 2
    it "lookup a valid cParam" $ do
      cParam instruction intCodeX `shouldBe` 1
