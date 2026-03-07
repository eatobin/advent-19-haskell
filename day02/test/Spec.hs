import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IntMap
import Lib (IntCodeStruct (..), makeMemory, pr, pw)
import Test.Hspec (anyErrorCall, describe, hspec, it, shouldBe, shouldThrow)

main :: IO ()
main = hspec $ do
  describe "\nJust test if tests work" $ do
    it "a test all by itself" $ do
      (78 :: Int) `shouldBe` (78 :: Int)

  describe "\nIntCodeStruct Tests" $ do
    let memoryAsCSVString = "10,11,1"
    let thisMemory = IntMap.fromList [(0, 10), (1, 11), (2, 1)]
    let intCode = IntCode {pointer = 0, memory = thisMemory}

    it "make a Memory" $ do
      makeMemory memoryAsCSVString `shouldBe` thisMemory
    it "lookup a valid Memory index - pw" $ do
      pw intCode 2 `shouldBe` 1
    it "lookup a valid Memory index - pr" $ do
      pr intCode 2 `shouldBe` 11
    it "lookup an invalid Memory index" $ do
      evaluate (pw intCode 33) `shouldThrow` anyErrorCall
