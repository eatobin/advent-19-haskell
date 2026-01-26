import Lib (gas, gasPlus)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  describe "\nStand alone test" $ do
    it "all by itself" $ do
      (78 :: Int) `shouldBe` (78 :: Int)

  describe "\nPart A" $ do
    it "gas at 6 should pass with zero" $ do
      gas 6 `shouldBe` 0
    it "gas at 9" $ do
      gas 9 `shouldBe` 1
    it "gas at 12" $ do
      gas 12 `shouldBe` 2
    it "gas at 14" $ do
      gas 14 `shouldBe` 2
    it "gas at 1969" $ do
      gas 1969 `shouldBe` 654
    it "gas at 100756" $ do
      gas 100756 `shouldBe` 33583

  describe "\nPart B" $ do
    it "gasPlus at 6 should pass with zero" $ do
      gasPlus 6 `shouldBe` 0
    it "gasPlus at 9" $ do
      gasPlus 9 `shouldBe` 1
    it "gasPlus at 12" $ do
      gasPlus 12 `shouldBe` 2
    it "gasPlus at 14" $ do
      gasPlus 14 `shouldBe` 2
    it "gasPlus at 1969" $ do
      gasPlus 1969 `shouldBe` 966
    it "gasPlus at 100756" $ do
      gasPlus 100756 `shouldBe` 50346
