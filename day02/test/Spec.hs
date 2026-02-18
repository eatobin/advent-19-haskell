import qualified Data.IntMap.Strict as IM
import Lib (IntCodeStruct (..), lookUpFromMemory, makeIntcode)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Just test if tests work" $ do
    it "a test all by itself" $ do
      (78 :: Int) `shouldBe` (78 :: Int)

  describe "\nIntCodeStruct Tests" $ do
    let memoryAsCSVString = "20,21,22,23,24"
    let intCode = IntCode {pointer = 2, memory = IM.fromList [(0, 20), (1, 21), (2, 22), (3, 23), (4, 24)]}
    it "make an IntCodeStruct" $ do
      makeIntcode 2 memoryAsCSVString `shouldBe` intCode
    it "lookup a Memory value" $ do
      lookUpFromMemory intCode 3 `shouldBe` 23
