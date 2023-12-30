module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Day211 (toFood, Food(..))


testFood = TestCase $ assertEqual "toFood should parse ingredients and allergens correctly"
    (Food ["eggs", "milk", "peanuts"] ["dairy", "nuts"]) (toFood ("eggs milk peanuts", "(contains dairy nuts)"))

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [TestLabel "testFood" testFood]

main = defaultMain tests

--import Day211 (toFood, Food(..))
--
---- Define test cases
--testToFood :: Test
--testToFood = test
--  [ "toFood should parse ingredients and allergens correctly" ~:
--    do
--      let input = ("eggs milk peanuts", "(contains dairy nuts)")
--      let expectedOutput = Food ["eggs", "milk", "peanuts"] ["dairy", "nuts"]
--      assertEqual "Test Case 1" expectedOutput (toFood input)
--
--    -- Add more test cases as needed
--  ]
--
---- Run the test suite
--main :: IO Counts
--main = do
--  runTestTT testToFood
