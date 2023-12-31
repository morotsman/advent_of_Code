module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import Day (scoreGame, Player(..), parseRows)

testParseRows = TestCase $ do
  let input = [
        "Player 1:",
        "9",
        "2",
        "6",
        "3",
        "1",
        "",
        "Player 2:",
        "5",
        "8",
        "4",
        "7",
        "10"
        ]
  let expected = [
        Player "Player 1:" [9,2,6,3,1],
        Player "Player 2:" [5,8,4,7,10]
        ]
  let actual = parseRows input
  assertEqual "" expected actual

testScoreGame = TestCase $ do
  let input = [
        "Player 1:",
        "9",
        "2",
        "6",
        "3",
        "1",
        "",
        "Player 2:",
        "5",
        "8",
        "4",
        "7",
        "10"
        ]
  let expected = 306
  let actual = scoreGame $ playGame $ getPlayers input
  assertEqual "" expected actual

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
  TestLabel "testParseRows" testParseRows,
  TestLabel "testScoreGame" testScoreGame
  ]

main = defaultMain tests
