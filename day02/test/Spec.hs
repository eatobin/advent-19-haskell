import Lib (IntCodeStruct (..), Memory, Pointer, makeMemoryAsList, myReadToInt)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "Just test if tests work" $ do
    it "a test all by itself" $ do
      (78 :: Int) `shouldBe` (78 :: Int)

  describe "\nMake an IntCodeStruct" $ do
    it "make a MemoryAsList" $ do
      let memoryAsCSVString = "10,11,12,13,14"
      makeMemoryAsList memoryAsCSVString `shouldBe` [(0, 10), (1, 11), (2, 12), (3, 13), (4, 14)]
