-- file Spec.hs From Eric!
import Lib (gas, someFunc)
import Test.Hspec (hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  it "works" $ do
    someFunc `shouldBe` 101
  it "all by itself" $ do
    (78 :: Int) `shouldBe` (78 :: Int)
  it "gas at 12" $ do
    gas 12 `shouldBe` 2
  it "gas at 14" $ do
    gas 14 `shouldBe` 2
  it "gas at 1969" $ do
    gas 1969 `shouldBe` 654
  it "gas at 100756" $ do
    gas 100756 `shouldBe` 33583
