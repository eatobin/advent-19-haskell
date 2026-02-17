import qualified Data.Map as M
import Lib (IntCodeStruct (..), makeIntcode)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Just test if tests work" $ do
    it "a test all by itself" $ do
      (78 :: Int) `shouldBe` (78 :: Int)

  describe "\nMake an IntCodeStruct" $ do
    it "make an IntCodeStruct" $ do
      let memoryAsCSVString = "20,21,22,23,24"
      makeIntcode 20 memoryAsCSVString `shouldBe` IntCode {pointer = 20, memory = M.fromList [(0, 20), (1, 21), (2, 22), (3, 23), (4, 24)]}
