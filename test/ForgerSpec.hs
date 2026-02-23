module ForgerSpec (spec) where

import Data.Set qualified as Set
import Forger
import Support.Fixture
import Test.Hspec

spec :: Spec
spec = do
  describe "forge" $ do
    it "should create an instance" $ do
      person <- forge personMold Nothing
      name person `shouldSatisfy` (not . null)

    it "should allow overrides" $ do
      let overrides = PersonOverrides (Just "Custom") Nothing
      person <- forge personMold (Just overrides)
      name person `shouldBe` "Custom"

    it "should produce unique instances on each call" $ do
      person1 <- forge personMold Nothing
      person2 <- forge personMold Nothing
      name person1 `shouldNotBe` name person2

  describe "forgeMulti" $ do
    it "should create multiple instances" $ do
      people <- forgeMulti personMold 5 Nothing
      length people `shouldBe` 5

    it "should produce unique instances" $ do
      people <- forgeMulti personMold 10 Nothing
      let uniqueNames = Set.fromList (map name people)
      Set.size uniqueNames `shouldBe` 10

    it "should apply overrides to all instances" $ do
      let overrides = PersonOverrides Nothing (Just 25)
      people <- forgeMulti personMold 3 (Just overrides)
      all (\person -> age person == 25) people `shouldBe` True

  describe "forgeWithSeed" $ do
    it "should create an instance with specific seed" $ do
      let person = forgeWithSeed personMold Nothing 42
      name person `shouldSatisfy` (not . null)
      age person `shouldBe` 42

    it "should produce consistent results for the same seed" $ do
      let person1 = forgeWithSeed personMold Nothing 100
          person2 = forgeWithSeed personMold Nothing 100
      person1 `shouldBe` person2

    it "should allow overrides" $ do
      let overrides = PersonOverrides (Just "Custom") Nothing
          person = forgeWithSeed personMold (Just overrides) 42
      name person `shouldBe` "Custom"
      age person `shouldBe` 42

  describe "forgeMultiWithSeed" $ do
    it "should create multiple instances starting from seed" $ do
      let people = forgeMultiWithSeed personMold 3 Nothing 100
      length people `shouldBe` 3
      all (not . null . name) people `shouldBe` True

    it "should produce consistent results" $ do
      let people1 = forgeMultiWithSeed personMold 5 Nothing 42
          people2 = forgeMultiWithSeed personMold 5 Nothing 42
      people1 `shouldBe` people2

    it "should apply overrides to all instances" $ do
      let overrides = PersonOverrides Nothing (Just 30)
          people = forgeMultiWithSeed personMold 3 (Just overrides) 100
      all (\person -> age person == 30) people `shouldBe` True

  describe "forge with StringMold" $ do
    it "should create strings" $ do
      let mold = stringMold (Just 5) (Just 10) Nothing
      result <- forge mold Nothing
      length result `shouldSatisfy` (>= 5)
      length result `shouldSatisfy` (<= 10)

    it "should create multiple unique strings" $ do
      let mold = stringMold (Just 10) (Just 20) Nothing
      results <- forgeMulti mold 10 Nothing
      let uniqueResults = Set.fromList results
      Set.size uniqueResults `shouldBe` 10
