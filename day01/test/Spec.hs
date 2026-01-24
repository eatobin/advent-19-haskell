import Control.Exception (evaluate)
import Lib (gas)
import Numeric.Natural (Natural)
import Test.Hspec (anyException, hspec, it, shouldBe, shouldThrow)

main :: IO ()
main = hspec $ do
  it "all by itself" $ do
    (78 :: Natural) `shouldBe` (78 :: Natural)
  it "gas at 0 should fail" $ do
    evaluate (gas 0) `shouldThrow` anyException
  it "gas at 3 should fail" $ do
    evaluate (gas 3) `shouldThrow` anyException
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
