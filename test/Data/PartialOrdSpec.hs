module Data.PartialOrdSpec (spec) where

import Test.Hspec
import Data.PartialOrd


spec :: Spec
spec = do

  describe "Maxima and minima" $ do

    it "should compute maxima" $ do
      maxima [(i,j) | i <- [1..10], j <- [1..10], i+j <= 10] `shouldBe`
        [(i :: Int,10-i) | i <- [1..9]]

    it "should compute minima" $ do
      minima [(i,j) | i <- [1..10], j <- [1..10], i+j >= 10] `shouldBe`
        [(i :: Int,10-i) | i <- [1..9]]

