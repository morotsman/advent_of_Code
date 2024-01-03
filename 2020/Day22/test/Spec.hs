module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import Day (scoreRecursiveGame, Deck, RecursivePlayer(..), getRecursivePlayers, playGameRecursiveCombat, getPlayers, playGame, scoreGame, Player(..), parseRows)

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
  assertEqual "The correct score should be 306" expected actual

testPlayGameRecursiveCombat = TestCase $ do
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
  let winningDeck = [7,5,6,2,4,1,10,8,9,3]
  let (player1, player2) = getRecursivePlayers input
  let (finalPlayer1, finalPlayer2) = playGameRecursiveCombat player1 player2
  assertEqual "The correct winner should be Player 2" winningDeck (head (decks finalPlayer2))

testScoreRecursiveGame = TestCase $ do
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
  let expected = 291
  let (player1, player2) = getRecursivePlayers input
  let actual = scoreRecursiveGame $ playGameRecursiveCombat player1 player2
  assertEqual "The correct score should be 291" expected actual

testInfiniteLoop = TestCase $ do
  let input = [
        "Player 1:",
        "43",
        "19",
        "",
        "Player 2:",
        "2",
        "29",
        "14"
        ]
  let expected = 105
  let (player1, player2) = getRecursivePlayers input
  let actual = scoreRecursiveGame $ playGameRecursiveCombat player1 player2
  assertEqual "The correct score should be 105" expected actual

-- hUnitTestToTests: Adapt an existing HUnit test into a list of test-framework tests
tests = hUnitTestToTests $ TestList [
  TestLabel "testParseRows" testParseRows,
  TestLabel "testScoreGame" testScoreGame,
  TestLabel "testPlayGameRecursiveCombat" testPlayGameRecursiveCombat,
  TestLabel "testInfiniteLoop" testInfiniteLoop
  ]

main = defaultMain tests
