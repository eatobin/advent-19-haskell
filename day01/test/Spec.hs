import Control.Exception (evaluate)
import Lib (gasA)
import Numeric.Natural (Natural)
import Test.Hspec (anyException, hspec, it, shouldBe, shouldThrow)

main :: IO ()
main = hspec $ do
  it "all by itself" $ do
    (78 :: Natural) `shouldBe` (78 :: Natural)
  it "gasA at 0 should fail" $ do
    evaluate (gasA 0) `shouldThrow` anyException
  it "gasA at 3 should fail" $ do
    evaluate (gasA 3) `shouldThrow` anyException
  it "gasA at 6 should pass with zero" $ do
    gasA 6 `shouldBe` 0
  it "gasA at 9" $ do
    gasA 9 `shouldBe` 1
  it "gasA at 12" $ do
    gasA 12 `shouldBe` 2
  it "gasA at 14" $ do
    gasA 14 `shouldBe` 2
  it "gasA at 1969" $ do
    gasA 1969 `shouldBe` 654
  it "gasA at 100756" $ do
    gasA 100756 `shouldBe` 33583
