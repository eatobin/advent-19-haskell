import Lib (makeIntcode)
import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  it "all by itself" $ do
    (78 :: Int) `shouldBe` (78 :: Int)

  describe "\nMake an IntCode" $ do
    it "intCode pointer is 20" $ do
      let kk = makeIntcode 22 "10,20"
      pointer kk `shouldBe` 20

-- pointer :: Lib.IntCode -> Int
-- pointer = pointer Lib.intCode
