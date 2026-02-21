import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IM
import Lib (IntCodeStruct (..), keyToKey, makeIntcode)
import Test.Hspec (anyErrorCall, describe, hspec, it, shouldBe, shouldThrow)

main :: IO ()
main = hspec $ do
  describe "\nJust test if tests work" $ do
    it "a test all by itself" $ do
      (78 :: Int) `shouldBe` (78 :: Int)

  describe "\nIntCodeStruct Tests" $ do
    let memoryAsCSVString = "20,21,22,23,24"
    let intCode = IntCode {pointer = 0, memory = IM.fromList [(0, 20), (1, 21), (2, 22), (3, 23), (4, 24)]}
    it "make an IntCodeStruct" $ do
      makeIntcode 0 memoryAsCSVString `shouldBe` intCode
    it "lookup a valid Memory index" $ do
      keyToKey intCode 3 `shouldBe` 23
    it "lookup an invalid Memory index" $ do
      evaluate (keyToKey intCode 33) `shouldThrow` anyErrorCall
