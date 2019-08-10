import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lib
import GHC.Real

main :: IO ()
main = hspec $ do
  describe "Lib.hs" $ do
    it "calculates sum of 1 and 1" $ do
      calculateFinalNumber (2^1*3^1) sumProgram `shouldBe` (Just $ 3^2)
    it "substracts 62 from 65" $ do
      calculateFinalNumber (2^65*3^62) subtractionProgram `shouldBe` (Just $ 2^3)
