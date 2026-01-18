-- file Spec.hs From Eric!
import Lib (someFunc)
import Test.Hspec

main :: IO ()
main = hspec $ do
  it "works" $ do
    someFunc `shouldBe` 101
  it "all by itself" $ do
    (78 :: Int) `shouldBe` (78 :: Int)
