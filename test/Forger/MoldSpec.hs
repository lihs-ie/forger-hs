module Forger.MoldSpec (spec) where

import Control.Exception (evaluate)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Forger
import Support.Fixture
import Test.Hspec

spec :: Spec
spec = do
  describe "Mold" $ do
    it "should create an instance with seed" $ do
      let person = pour personMold Nothing 42
      name person `shouldBe` "f72AEwGYD1fDBwke4cGRFDzV84mygoPdDnjWSsYgsFt"
      age person `shouldBe` 42

    it "should allow overrides" $ do
      let overrides = PersonOverrides (Just "Custom") Nothing
          person = pour personMold (Just overrides) 42
      name person `shouldBe` "Custom"
      age person `shouldBe` 42

    it "should produce consistent results for the same seed" $ do
      let person1 = pour personMold Nothing 100
          person2 = pour personMold Nothing 100
      person1 `shouldBe` person2

  describe "StringMold" $ do
    it "should create a string with default settings" $ do
      let mold = stringMold Nothing Nothing Nothing
          result = pour mold Nothing 10
      length result `shouldSatisfy` (>= 1)
      length result `shouldSatisfy` (<= 255)

    it "should respect minimum and maximum length" $ do
      let mold = stringMold (Just 5) (Just 10) Nothing
          result = pour mold Nothing 0
      length result `shouldSatisfy` (>= 5)
      length result `shouldSatisfy` (<= 10)

    it "should use custom characters" $ do
      let mold = stringMold (Just 10) (Just 10) (Just ['x'])
          result = pour mold Nothing 42
      result `shouldBe` "xxxxxxxxxx"

    it "should produce consistent results for the same seed" $ do
      let mold = stringMold Nothing Nothing Nothing
          result1 = pour mold Nothing 42
          result2 = pour mold Nothing 42
      result1 `shouldBe` result2

    it "should allow value override" $ do
      let mold = stringMold Nothing Nothing Nothing
          result = pour mold (Just (StringOverrides (Just "custom"))) 42
      result `shouldBe` "custom"

    it "should use ALPHA characters when specified" $ do
      let mold = stringMold (Just 10) (Just 10) (Just alpha)
          result = pour mold Nothing 42
      all (`elem` alpha) result `shouldBe` True

    it "should use NUMERIC characters when specified" $ do
      let mold = stringMold (Just 10) (Just 10) (Just numeric)
          result = pour mold Nothing 42
      all (`elem` numeric) result `shouldBe` True

  describe "EnumMold" $ do
    let animals = [minBound .. maxBound] :: [Animal]

    it "should create a value from enum" $ do
      let result = pour animalMold Nothing 0
      result `shouldSatisfy` (`elem` animals)

    it "should produce consistent results for the same seed" $ do
      let result1 = pour animalMold Nothing 42
          result2 = pour animalMold Nothing 42
      result1 `shouldBe` result2

    it "should allow value override" $ do
      let overrides = EnumOverrides (Just Dog) NoExclusion
          result = pour animalMold (Just overrides) 42
      result `shouldBe` Dog

    it "should exclude single value" $ do
      let overrides = EnumOverrides Nothing (ExcludeSingle Dog)
      all (\seed -> pour animalMold (Just overrides) seed /= Dog) [0 .. 99] `shouldBe` True

    it "should exclude multiple values using Set" $ do
      let exclusions = ExcludeMultiple (Set.fromList [Dog, Cat])
          overrides = EnumOverrides Nothing exclusions
      all (\seed -> pour animalMold (Just overrides) seed == Bird) [0 .. 99] `shouldBe` True

    it "should throw when all candidates are excluded" $ do
      let exclusions = ExcludeMultiple (Set.fromList [Dog, Cat, Bird])
          overrides = EnumOverrides Nothing exclusions
      evaluate (pour animalMold (Just overrides) 0) `shouldThrow` anyErrorCall

    describe "MapMold" $ do
      let keyMold = stringMold (Just 5) (Just 5) Nothing
          valueMold = stringMold (Just 10) (Just 10) Nothing
          targetMold = mapMold keyMold valueMold

      it "should create a Map" $ do
        let result = pour targetMold Nothing 42
        Map.size result `shouldSatisfy` (> 0)
        Map.size result `shouldSatisfy` (<= 10)

      it "should produce consistent results for the same seed" $ do
        let result1 = pour targetMold Nothing 42
            result2 = pour targetMold Nothing 42
        result1 `shouldBe` result2

      it "should allow entries override" $ do
        let customEntries = [("key1", "value1"), ("key2", "value2")]
            overrides = MapOverrides (Just customEntries)
            result = pour targetMold (Just overrides) 42
        Map.lookup "key1" result `shouldBe` Just "value1"
        Map.lookup "key2" result `shouldBe` Just "value2"
