module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import Day211 (countNonSuspects, findNonSuspects, findAllAllergensInSuspects, findAllAllergens, parseRows, parseFood, toFood, Food(..), findSuspects, findAllSuspects)

testToFood = TestCase $ assertEqual "toFood should parse ingredients and allergens correctly"
    (Food (Set.fromList ["eggs", "milk", "peanuts"]) (Set.fromList["dairy", "nuts"])) (toFood ("eggs milk peanuts", "(contains dairy nuts)"))

testParseFood = TestCase $ assertEqual "parseFood should parse ingredients and allergens correctly"
    (Food (Set.fromList ["eggs", "milk", "peanuts"]) (Set.fromList["dairy", "nuts"])) (parseFood "eggs milk peanuts (contains dairy nuts)")

testParseFoodWithoutAllergens = TestCase $ assertEqual "parseFood should parse ingredients correctly"
    (Food (Set.fromList ["eggs", "milk", "peanuts"]) Set.empty) (parseFood "eggs milk peanuts")

testFindSuspects = TestCase $ assertEqual "findSuspects should find suspects correctly"
    (Map.fromList [("dairy", Set.fromList ["aaaa", "bbbb"])]) (findSuspects (parseFood "aaaa bbbb (contains dairy)"))

testFindAllSuspectsOnlyOneFood = TestCase $ assertEqual "findAllSuspects should find suspects correctly"
    (Map.fromList [("dairy", Set.fromList ["aaaa", "bbbb"]), ("nuts", Set.fromList ["aaaa", "bbbb"])]) (findAllSuspects [parseFood "aaaa bbbb (contains dairy nuts)"])

testFindAllSuspects = TestCase $ assertEqual "findAllSuspects should find suspects correctly with several foods"
    (Map.fromList [("dairy", Set.fromList ["aaaa", "bbbb"]), ("nuts", Set.fromList ["aaaa", "bbbb"])]) (findAllSuspects $ parseRows ["aaaa bbbb (contains dairy)", "aaaa bbbb (contains dairy nuts)"])

testFindAllSuspects2 = TestCase $ assertEqual "findAllSuspects should find suspects correctly with several foods"
    (Map.fromList [("dairy", Set.fromList ["aaaa"]), ("nuts", Set.fromList ["aaaa", "bbbb"])]) (findAllSuspects $ parseRows ["aaaa (contains dairy)", "aaaa bbbb (contains dairy nuts)"])

testFindAllSuspects3 = TestCase $ assertEqual "findAllSuspects should find suspects correctly with several foods"
    (Map.fromList [("dairy", Set.fromList ["aaaa"]), ("nuts", Set.fromList ["aaaa", "bbbb"]), ("soy", Set.fromList ["cccc"])]) (findAllSuspects $ parseRows ["aaaa (contains dairy)", "aaaa bbbb (contains dairy, nuts)", "cccc bbbb (contains soy)", "cccc (contains soy)"])

testFindAllSuspects4 = TestCase $ assertEqual "findAllSuspects should find suspects correctly with several foods"
    (Map.fromList [("dairy", Set.fromList ["mxmxvkd"]), ("fish", Set.fromList ["mxmxvkd","sqjhc"]), ("soy", Set.fromList ["fvjkl","sqjhc"])]) (findAllSuspects $ parseRows ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)", "trh fvjkl sbzzf mxmxvkd (contains dairy)", "sqjhc fvjkl (contains soy)", "sqjhc mxmxvkd sbzzf (contains fish)"])

testFindAllAllergens = TestCase $ assertEqual "findAllAllergens should find allergens correctly with several foods"
    (Set.fromList ["dairy", "fish", "soy"]) (findAllAllergens $ parseRows ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)", "trh fvjkl sbzzf mxmxvkd (contains dairy)", "sqjhc fvjkl (contains soy)", "sqjhc mxmxvkd sbzzf (contains fish)"])

testfFndAllAllergensInSuspects = TestCase $ assertEqual "findAllAllergensInSuspects should find allergens correctly with several foods"
    (Set.fromList ["dairy", "fish", "soy"]) (findAllAllergensInSuspects $ findAllSuspects $ parseRows ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)", "trh fvjkl sbzzf mxmxvkd (contains dairy)", "sqjhc fvjkl (contains soy)", "sqjhc mxmxvkd sbzzf (contains fish)"])

testFindNonSuspects = TestCase $ assertEqual "findNonSuspects should find non suspects correctly with several foods"
    (Set.fromList ["kfcds", "nhms", "sbzzf", "trh"]) (findNonSuspects $ parseRows ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)", "trh fvjkl sbzzf mxmxvkd (contains dairy)", "sqjhc fvjkl (contains soy)", "sqjhc mxmxvkd sbzzf (contains fish)"])

testCountNonSuspects = TestCase $ assertEqual "countNonSuspects should count non suspects correctly with several foods"
    5 (countNonSuspects $ parseRows ["mxmxvkd kfcds sqjhc nhms (contains dairy, fish)", "trh fvjkl sbzzf mxmxvkd (contains dairy)", "sqjhc fvjkl (contains soy)", "sqjhc mxmxvkd sbzzf (contains fish)"])

--mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
--trh fvjkl sbzzf mxmxvkd (contains dairy)
--sqjhc fvjkl (contains soy)
--sqjhc mxmxvkd sbzzf (contains fish)


-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
  TestLabel "testToFood" testToFood,
  TestLabel "testParseFood" testParseFood,
  TestLabel "testParseFoodWithoutAllergens" testParseFoodWithoutAllergens,
  TestLabel "testFindSuspects" testFindSuspects,
  TestLabel "testFindAllSuspectsOnlyOneFood" testFindAllSuspectsOnlyOneFood,
  TestLabel "testFindAllSuspects" testFindAllSuspects,
  TestLabel "testFindAllSuspects2" testFindAllSuspects2,
  TestLabel "testFindAllSuspects3" testFindAllSuspects3,
  TestLabel "testFindAllSuspects4" testFindAllSuspects4,
  TestLabel "testFindAllAllergens" testFindAllAllergens,
  TestLabel "testFindNonSuspects" testFindNonSuspects,
  TestLabel "testCountNonSuspects" testCountNonSuspects
  ]

main = defaultMain tests
