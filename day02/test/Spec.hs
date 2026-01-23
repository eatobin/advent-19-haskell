import Test.Hspec (hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  it "all by itself" $ do
    (78 :: Int) `shouldBe` (78 :: Int)
