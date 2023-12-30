module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Set as Set
import Day211 (parseFood, toFood, Food(..))

testToFood = TestCase $ assertEqual "toFood should parse ingredients and allergens correctly"
    (Food (Set.fromList ["eggs", "milk", "peanuts"]) (Set.fromList["dairy", "nuts"])) (toFood ("eggs milk peanuts", "(contains dairy nuts)"))

testParseFood = TestCase $ assertEqual "parseFood should parse ingredients and allergens correctly"
    (Food (Set.fromList ["eggs", "milk", "peanuts"]) (Set.fromList["dairy", "nuts"])) (parseFood "eggs milk peanuts (contains dairy nuts)")

testParseFoodWithoutAllergens = TestCase $ assertEqual "parseFood should parse ingredients correctly"
    (Food (Set.fromList ["eggs", "milk", "peanuts"]) Set.empty) (parseFood "eggs milk peanuts")

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
  TestLabel "testToFood" testToFood,
  TestLabel "testParseFood" testParseFood,
  TestLabel "testParseFoodWithoutAllergens" testParseFoodWithoutAllergens
  ]

main = defaultMain tests
