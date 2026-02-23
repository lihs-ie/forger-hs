module Forger.InternalSpec (spec) where

import Forger.Internal
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldSatisfy)
import Test.Hspec.Hedgehog (hedgehog)

spec :: Spec
spec = do
  describe "scramble" $ do
    it "should return consistent results for the same input" $ do
      scramble 42 `shouldBe` scramble 42

    it "should return different results for different inputs" $ do
      scramble 1 `shouldNotBe` scramble 2

    it "should handle zero" $ do
      let result = scramble 0
      result `shouldSatisfy` (>= 0)

    it "should handle large numbers" $ do
      let result = scramble maxBound
      result `shouldSatisfy` (>= 0)

    it "should be deterministic (property)" $ hedgehog $ do
      seed <- Hedgehog.forAll $ Gen.int (Range.linear 0 maxBound)
      scramble seed Hedgehog.=== scramble seed

    describe "Characters" $ do
      it "should have 62 ALPHANUMERIC characters" $ do
        length alphanumeric `shouldBe` 62
        'a' `elem` alphanumeric `shouldBe` True
        'Z' `elem` alphanumeric `shouldBe` True
        '0' `elem` alphanumeric `shouldBe` True
        '9' `elem` alphanumeric `shouldBe` True

      it "should have 52 ALPHA characters" $ do
        length alpha `shouldBe` 52
        'a' `elem` alpha `shouldBe` True
        'Z' `elem` alpha `shouldBe` True
        '0' `elem` alpha `shouldBe` False

      it "should have 37 SLUG characters" $ do
        length slug `shouldBe` 37
        'a' `elem` slug `shouldBe` True
        '-' `elem` slug `shouldBe` True
        'A' `elem` slug `shouldBe` False

      it "should have 10 NUMERIC characters" $ do
        length numeric `shouldBe` 10
        '0' `elem` numeric `shouldBe` True
        '9' `elem` numeric `shouldBe` True
        'a' `elem` numeric `shouldBe` False

      it "should have 32 SYMBOL characters" $ do
        length symbolCharacters `shouldBe` 32
        '!' `elem` symbolCharacters `shouldBe` True
        '@' `elem` symbolCharacters `shouldBe` True
