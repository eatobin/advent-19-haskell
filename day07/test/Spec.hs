import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import Lib (IntCodeStruct (..), aParam, add, bParam, cParam, makeInstruction, makeMemory, multiply, pr, pw, runOpCode)
import Test.Hspec (anyErrorCall, describe, hspec, it, shouldBe, shouldThrow)

-- IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = memory, stopped = False, recur = True}

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
  let intCode = IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = thisMemory, stopped = False, recur = True}
  let thisMemoryX = Map.fromList [(0, 0), (1, 1), (2, 2), (3 :: Int, 3 :: Int)]
  let intCodeX = IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = thisMemoryX, stopped = False, recur = True}
  let thisMemoryAddMult = Map.fromList [(0, 0), (1, 2), (2, 1), (3 :: Int, 0 :: Int)]
  let intCodeAddMult = IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = thisMemoryAddMult, stopped = False, recur = True}
  let intCodeAdd = IntCode {input = 0, output = 0, phase = 0, pointer = 4, memory = Map.fromList [(0, 3), (1, 2), (2, 1), (3, 0)], stopped = False, recur = True}
  let intCodeMult = IntCode {input = 0, output = 0, phase = 0, pointer = 4, memory = Map.fromList [(0, 2), (1, 2), (2, 1), (3, 0)], stopped = False, recur = True}
  let inputOutput :: String
      inputOutput = "3,0,4,0,99"
  let modesString = "1002,4,3,4,33"
  let equalEightPos = "3,9,8,9,10,9,4,9,99,-1,8"
  let lessThanEightPos = "3,9,7,9,10,9,4,9,99,-1,8"
  let equalEightImm = "3,3,1108,-1,8,3,4,3,99"
  let lessThanEightImm = "3,3,1107,-1,8,3,4,3,99"
  let jumpIfZeroPos = "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9"
  let jumpIfZeroImm = "3,3,1105,-1,9,1101,0,0,12,4,12,99,1"
  let largeEight = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

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
        runOpCode (IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = makeMemory aocMemory1, stopped = False, recur = True})
        `shouldBe` IntCode {input = 0, output = 0, phase = 0, pointer = 4, memory = Map.fromList [(0, 1), (1, 0), (2, 0), (3, 2), (4, 99)], stopped = False, recur = True}
    it "aocMemory2Test" $
      do
        runOpCode (IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = makeMemory aocMemory2, stopped = False, recur = True})
        `shouldBe` IntCode {input = 0, output = 0, phase = 0, pointer = 8, memory = Map.fromList [(0, 3500), (1, 9), (2, 10), (3, 70), (4, 2), (5, 3), (6, 11), (7, 0), (8, 99), (9, 30), (10, 40), (11, 50)], stopped = False, recur = True}
    it "aocMemory3Test" $
      do
        runOpCode (IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = makeMemory aocMemory3, stopped = False, recur = True})
        `shouldBe` IntCode {input = 0, output = 0, phase = 0, pointer = 4, memory = Map.fromList [(0, 2), (1, 0), (2, 0), (3, 0), (4, 99)], stopped = False, recur = True}
    it "aocMemory4Test" $
      do
        runOpCode (IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = makeMemory aocMemory4, stopped = False, recur = True})
        `shouldBe` IntCode {input = 0, output = 0, phase = 0, pointer = 4, memory = Map.fromList [(0, 2), (1, 3), (2, 0), (3, 6), (4, 99)], stopped = False, recur = True}
    it "aocMemory5Test" $
      do
        runOpCode (IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = makeMemory aocMemory5, stopped = False, recur = True})
        `shouldBe` IntCode {input = 0, output = 0, phase = 0, pointer = 4, memory = Map.fromList [(0, 2), (1, 4), (2, 4), (3, 5), (4, 99), (5, 9801)], stopped = False, recur = True}
    it "aocMemory6Test" $
      do
        runOpCode (IntCode {input = 0, output = 0, phase = 0, pointer = 0, memory = makeMemory aocMemory6, stopped = False, recur = True})
        `shouldBe` IntCode {input = 0, output = 0, phase = 0, pointer = 8, memory = Map.fromList [(0, 30), (1, 1), (2, 1), (3, 4), (4, 2), (5, 5), (6, 6), (7, 0), (8, 99)], stopped = False, recur = True}

-- describe "\ninput/output Tests" $ do
--   it "input/outputTest" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory inputOutput, input = 7, stopped = False, recur = True})
--       `shouldBe` IntCode {input = 7, output = 7, phase = 0, pointer = 4, memory = Map.fromList [(0, 7), (1, 0), (2, 4), (3, 0), (4, 99)], stopped = False, recur = True}

-- describe "\nmodes Tests" $ do
--   it "modesTest" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory modesString, input = 0}, stopped = False, recur = True)
--       `shouldBe` IntCode {input = 0, output = 0, phase = 0, pointer = 4, memory = Map.fromList [(0, 1002), (1, 4), (2, 3), (3, 4), (4, 99)], stopped = False, recur = True}

-- describe "\neights Tests" $ do
--   it "equalEightTruePos" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory equalEightPos, input = 8}, stopped = False, recur = True)
--       `shouldBe` IntCode {input = 8, output = 1, pointer = 8, phase = 0, memory = Map.fromList [(0, 3), (1, 9), (2, 8), (3, 9), (4, 10), (5, 9), (6, 4), (7, 9), (8, 99), (9, 1), (10, 8)], stopped = False, recur = True}
--   it "equalEightFalsePos" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory equalEightPos, input = 88})
--       `shouldBe` IntCode {input = 88, output = 0, phase = 0, pointer = 8, memory = Map.fromList [(0, 3), (1, 9), (2, 8), (3, 9), (4, 10), (5, 9), (6, 4), (7, 9), (8, 99), (9, 0), (10, 8)], stopped = False, recur = True}
--   it "lessThanEightTruePos" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory lessThanEightPos, input = 5, stopped = False, recur = True})
--       `shouldBe` IntCode {input = 5, output = 1, pointer = 8, memory = Map.fromList [(0, 3), (1, 9), (2, 7), (3, 9), (4, 10), (5, 9), (6, 4), (7, 9), (8, 99), (9, 1), (10, 8)], stopped = False, recur = True}
--   it "lessThanEightFalsePos" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory lessThanEightPos, input = 9})
--       `shouldBe` IntCode {input = 9, output = 0, phase = 0, pointer = 8, memory = Map.fromList [(0, 3), (1, 9), (2, 7), (3, 9), (4, 10), (5, 9), (6, 4), (7, 9), (8, 99), (9, 0), (10, 8)], stopped = False, recur = True}
--   it "equalEightTrueImm" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory equalEightImm, input = 8}, stopped = False, recur = True)
--       `shouldBe` IntCode {input = 8, output = 1, pointer = 8, memory = Map.fromList [(0, 3), (1, 3), (2, 1108), (3, 1), (4, 8), (5, 3), (6, 4), (7, 3), (8, 99)], stopped = False, recur = True}
--   it "equalEightFalseImm" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory equalEightImm, input = 88, stopped = False, recur = True})
--       `shouldBe` IntCode {input = 88, output = 0, phase = 0, pointer = 8, memory = Map.fromList [(0, 3), (1, 3), (2, 1108), (3, 0), (4, 8), (5, 3), (6, 4), (7, 3), (8, 99)], stopped = False, recur = True}
--   it "lessThanEightTrueImm" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory lessThanEightImm, input = 5, stopped = False, recur = True})
--       `shouldBe` IntCode {input = 5, output = 1, pointer = 8, phase = 0, memory = Map.fromList [(0, 3), (1, 3), (2, 1107), (3, 1), (4, 8), (5, 3), (6, 4), (7, 3), (8, 99)], stopped = False, recur = True}
--   it "lessThanEightFalseImm" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory lessThanEightImm, input = 9})
--       `shouldBe` IntCode {input = 9, output = 0, phase = 0, pointer = 8, memory = Map.fromList [(0, 3), (1, 3), (2, 1107), (3, 0), (4, 8), (5, 3), (6, 4), (7, 3), (8, 99)], stopped = False, recur = True}

-- describe "\njump Tests" $ do
--   it "jumpIfZeroTruePos" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory jumpIfZeroPos, input = 0, stopped = False, recur = True})
--       `shouldBe` IntCode {input = 0, output = 0, phase = 0, pointer = 11, memory = Map.fromList [(0, 3), (1, 12), (2, 6), (3, 12), (4, 15), (5, 1), (6, 13), (7, 14), (8, 13), (9, 4), (10, 13), (11, 99), (12, 0), (13, 0), (14, 1), (15, 9)], stopped = False, recur = True}

--   it "jumpIfZeroFalsePos" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory jumpIfZeroPos, input = 1})
--       `shouldBe` IntCode {input = 1, output = 1, pointer = 11, memory = Map.fromList [(0, 3), (1, 12), (2, 6), (3, 12), (4, 15), (5, 1), (6, 13), (7, 14), (8, 13), (9, 4), (10, 13), (11, 99), (12, 1), (13, 1), (14, 1), (15, 9)], stopped = False, recur = True}
--   it "jumpIfZeroTrueImm" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory jumpIfZeroImm, input = 0})
--       `shouldBe` IntCode {input = 0, output = 0, phase = 0, pointer = 11, memory = Map.fromList [(0, 3), (1, 3), (2, 1105), (3, 0), (4, 9), (5, 1101), (6, 0), (7, 0), (8, 12), (9, 4), (10, 12), (11, 99), (12, 0)], stopped = False, recur = True}

--   it "jumpIfZeroFalseImm" $
--     do
--       runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory jumpIfZeroImm, input = 1})
--       `shouldBe` IntCode {input = 1, output = 1, pointer = 11, memory = Map.fromList [(0, 3), (1, 3), (2, 1105), (3, 1), (4, 9), (5, 1101), (6, 0), (7, 0), (8, 12), (9, 4), (10, 12), (11, 99), (12, 1)], stopped = False, recur = True}

--   describe "\nlargeEights Tests" $ do
--     it "lessThanEight" $
--       do
--         runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory largeEight, input = 7})
--         `shouldBe` IntCode {input = 7, output = 999, pointer = 46, memory = Map.fromList [(0, 3), (1, 21), (2, 1008), (3, 21), (4, 8), (5, 20), (6, 1005), (7, 20), (8, 22), (9, 107), (10, 8), (11, 21), (12, 20), (13, 1006), (14, 20), (15, 31), (16, 1106), (17, 0), (18, 36), (19, 98), (20, 0), (21, 7), (22, 1002), (23, 21), (24, 125), (25, 20), (26, 4), (27, 20), (28, 1105), (29, 1), (30, 46), (31, 104), (32, 999), (33, 1105), (34, 1), (35, 46), (36, 1101), (37, 1000), (38, 1), (39, 20), (40, 4), (41, 20), (42, 1105), (43, 1), (44, 46), (45, 98), (46, 99)], stopped = False, recur = True}
--     it "equalEight" $
--       do
--         runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory largeEight, input = 8})
--         `shouldBe` IntCode {input = 8, output = 1000, pointer = 46, memory = Map.fromList [(0, 3), (1, 21), (2, 1008), (3, 21), (4, 8), (5, 20), (6, 1005), (7, 20), (8, 22), (9, 107), (10, 8), (11, 21), (12, 20), (13, 1006), (14, 20), (15, 31), (16, 1106), (17, 0), (18, 36), (19, 98), (20, 1000), (21, 8), (22, 1002), (23, 21), (24, 125), (25, 20), (26, 4), (27, 20), (28, 1105), (29, 1), (30, 46), (31, 104), (32, 999), (33, 1105), (34, 1), (35, 46), (36, 1101), (37, 1000), (38, 1), (39, 20), (40, 4), (41, 20), (42, 1105), (43, 1), (44, 46), (45, 98), (46, 99)], stopped = False, recur = True}
--     it "greaterThanEight" $
--       do
--         runOpCode (IntCode {pointer = 0, output = 0, phase = 0, memory = makeMemory largeEight, input = 9})
--         `shouldBe` IntCode {input = 9, output = 1001, pointer = 46, memory = Map.fromList [(0, 3), (1, 21), (2, 1008), (3, 21), (4, 8), (5, 20), (6, 1005), (7, 20), (8, 22), (9, 107), (10, 8), (11, 21), (12, 20), (13, 1006), (14, 20), (15, 31), (16, 1106), (17, 0), (18, 36), (19, 98), (20, 1001), (21, 9), (22, 1002), (23, 21), (24, 125), (25, 20), (26, 4), (27, 20), (28, 1105), (29, 1), (30, 46), (31, 104), (32, 999), (33, 1105), (34, 1), (35, 46), (36, 1101), (37, 1000), (38, 1), (39, 20), (40, 4), (41, 20), (42, 1105), (43, 1), (44, 46), (45, 98), (46, 99)]}
