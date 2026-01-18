-- file Spec.hs From Eric!
import Lib (someFunc)
import Test.Hspec

main :: IO ()
main = hspec $ do
    it "works" $ do
      someFunc `shouldBe` (101 :: Int)
