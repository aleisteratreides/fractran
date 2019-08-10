import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import GHC.Real

main :: IO ()
main = hspec $ do
  describe "Lib.hs" $ do
    it "calculates sum of 1 and 1" $ do
      calculateFinalNumber 6 [3 % 2] `shouldBe` (Just 9)
