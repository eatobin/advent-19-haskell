import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import Lib (IntCodeStruct (..), aParam, add, bParam, cParam, makeInstruction, makeMemory, multiply, pr, pw, runOpCode)
import Test.Hspec (anyErrorCall, describe, hspec, it, shouldBe, shouldThrow)

main :: IO ()
main = hspec $ do
  let instruction1 = Map.fromList [('a', 0), ('b', 0), ('c', 0), ('d', 0), ('e', 6 :: Int)]
  let instruction3 = Map.fromList [('a', 0), ('b', 0), ('c', 4), ('d', 5), ('e', 6 :: Int)]
  let instruction5 = Map.fromList [('a', 2), ('b', 3), ('c', 4), ('d', 5), ('e', 6 :: Int)]
  let memoryAsCSVString = "10,11,1"
  let aocMemory1 = "1,0,0,3,99"
  let aocMemory2 = "1,9,10,3,2,3,11,0,99,30,40,50"
  let aocMemory3 = "1,0,0,0,99"
  let aocMemory4 = "2,3,0,3,99"
  let aocMemory5 = "2,4,4,5,99,0"
  let aocMemory6 = "1,1,1,4,99,5,6,0,99"
  let thisMemory = Map.fromList [(0, 10), (1, 11), (2 :: Int, 1 :: Int)]
  let intCode = IntCode {input = 0, pointer = 0, memory = thisMemory}
  let thisMemoryX = Map.fromList [(0, 0), (1, 1), (2, 2), (3 :: Int, 3 :: Int)]
  let intCodeX = IntCode {input = 0, pointer = 0, memory = thisMemoryX}
  let thisMemoryAddMult = Map.fromList [(0, 0), (1, 2), (2, 1), (3 :: Int, 0 :: Int)]
  let intCodeAddMult = IntCode {input = 0, pointer = 0, memory = thisMemoryAddMult}
  let intCodeAdd = IntCode {input = 0, pointer = 4, memory = Map.fromList [(0, 3), (1, 2), (2, 1), (3, 0)]}
  let intCodeMult = IntCode {input = 0, pointer = 4, memory = Map.fromList [(0, 2), (1, 2), (2, 1), (3, 0)]}

  describe "\nJust test if tests work" $ do
    it "a test all by itself" $ do
      (78 :: Int) `shouldBe` (78 :: Int)

  describe "\nInstruction Tests" $ do
    it "make an Instruction 1" $ do
      makeInstruction 6 `shouldBe` instruction1
    it "make an Instruction 3" $ do
      makeInstruction 456 `shouldBe` instruction3
    it "make an Instruction 5" $ do
      makeInstruction 23456 `shouldBe` instruction5

  describe "\nIntCodeStruct Tests" $ do
    it "make a Memory" $ do
      makeMemory memoryAsCSVString `shouldBe` thisMemory
    it "lookup a valid Memory index - pw" $ do
      pw intCode 2 `shouldBe` 1
    it "lookup a valid Memory index - pr" $ do
      pr intCode 2 `shouldBe` 11
    it "lookup an invalid Memory index" $ do
      evaluate (pw intCode 33) `shouldThrow` anyErrorCall

  describe "\nxParam Tests" $ do
    it "lookup a valid aParam" $ do
      aParam instruction1 intCodeX `shouldBe` 3
    it "lookup a valid bParam" $ do
      bParam instruction1 intCodeX `shouldBe` 2
    it "lookup a valid cParam" $ do
      cParam instruction1 intCodeX `shouldBe` 1

  describe "\nAdd/Mult Tests" $ do
    it "1 plus 2 should be set at 0 and pointer should be 4" $ do
      add instruction1 intCodeAddMult `shouldBe` intCodeAdd
    it "1 times 2 should be set at 0 and pointer should be 4" $ do
      multiply instruction1 intCodeAddMult `shouldBe` intCodeMult

  describe "\nrunOpCode Tests" $ do
    it "aocMemory1Test" $
      do
        runOpCode (IntCode {input = 0, pointer = 0, memory = makeMemory aocMemory1})
        `shouldBe` IntCode {input = 0, pointer = 4, memory = Map.fromList [(0, 1), (1, 0), (2, 0), (3, 2), (4, 99)]}
    it "aocMemory2Test" $
      do
        runOpCode (IntCode {input = 0, pointer = 0, memory = makeMemory aocMemory2})
        `shouldBe` IntCode {input = 0, pointer = 8, memory = Map.fromList [(0, 3500), (1, 9), (2, 10), (3, 70), (4, 2), (5, 3), (6, 11), (7, 0), (8, 99), (9, 30), (10, 40), (11, 50)]}
    it "aocMemory3Test" $
      do
        runOpCode (IntCode {input = 0, pointer = 0, memory = makeMemory aocMemory3})
        `shouldBe` IntCode {input = 0, pointer = 4, memory = Map.fromList [(0, 2), (1, 0), (2, 0), (3, 0), (4, 99)]}
    it "aocMemory4Test" $
      do
        runOpCode (IntCode {input = 0, pointer = 0, memory = makeMemory aocMemory4})
        `shouldBe` IntCode {input = 0, pointer = 4, memory = Map.fromList [(0, 2), (1, 3), (2, 0), (3, 6), (4, 99)]}
    it "aocMemory5Test" $
      do
        runOpCode (IntCode {input = 0, pointer = 0, memory = makeMemory aocMemory5})
        `shouldBe` IntCode {input = 0, pointer = 4, memory = Map.fromList [(0, 2), (1, 4), (2, 4), (3, 5), (4, 99), (5, 9801)]}
    it "aocMemory6Test" $
      do
        runOpCode (IntCode {input = 0, pointer = 0, memory = makeMemory aocMemory6})
        `shouldBe` IntCode {input = 0, pointer = 8, memory = Map.fromList [(0, 30), (1, 1), (2, 1), (3, 4), (4, 2), (5, 5), (6, 6), (7, 0), (8, 99)]}
