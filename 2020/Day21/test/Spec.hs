import Test.HUnit

-- Import the module containing the toFood function
import Day21_1 (toFood, Food(..))

-- Define test cases
testToFood :: Test
testToFood = test
  [ "toFood should parse ingredients and allergens correctly" ~:
    do
      let input = ("eggs milk peanuts", "(contains dairy nuts)")
      let expectedOutput = Food ["eggs", "milk", "peanuts"] ["dairy", "nuts"]
      assertEqual "Test Case 1" expectedOutput (toFood input)

    -- Add more test cases as needed
  ]

-- Run the test suite
main :: IO ()
main = do
  runTestTT testToFood
