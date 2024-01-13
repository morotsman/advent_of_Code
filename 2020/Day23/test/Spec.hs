module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import qualified Data.Set as Set
import qualified Data.Map as Map
import Day (playGame, indexesToPickUp, pickUp, selectDestinationCup)

testPlayGameFor1Rounds = TestCase $ assertEqual "playGameFor1Rounds" 328915467 (playGame 1 389125467)
testPlayGameFor2Rounds = TestCase $ assertEqual "playGameFor2Rounds" 325467891 (playGame 2 389125467)
testPlayGameFor3Rounds = TestCase $ assertEqual "playGameFor3Rounds" 725891346 (playGame 3 389125467)
testPlayGameFor4Rounds = TestCase $ assertEqual "playGameFor4Rounds" 325846791 (playGame 4 389125467)
testPlayGameFor5Rounds = TestCase $ assertEqual "playGameFor5Rounds" 925841367  (playGame 5 389125467)
testPlayGameFor6Rounds = TestCase $ assertEqual "playGameFor6Rounds" 725841936  (playGame 6 389125467)
testPlayGameFor7Rounds = TestCase $ assertEqual "playGameFor7Rounds" 836741925  (playGame 7 389125467)
testPlayGameFor8Rounds = TestCase $ assertEqual "playGameFor8Rounds" 741583926  (playGame 8 389125467)
testPlayGameFor9Rounds = TestCase $ assertEqual "playGameFor9Rounds" 574183926  (playGame 9 389125467)
testPlayGameFor10Rounds = TestCase $ assertEqual "playGameFor10Rounds" 583741926 (playGame 10 389125467)
testPlayGameFor100Rounds = TestCase $ assertEqual "playGameFor100Rounds" 916738452 (playGame 100 389125467)

testIndexesToPickUpAt0 = TestCase $ assertEqual "testIndexesToPickUp" [1,2,3] (indexesToPickUp 3 0 [3,8,9,1,2,5,4,6,7])
testIndexesToPickUpAt5 = TestCase $ assertEqual "testIndexesToPickUp" [6,7,8] (indexesToPickUp 3 5 [3,8,9,1,2,5,4,6,7])
testIndexesToPickUpAt6 = TestCase $ assertEqual "testIndexesToPickUp" [7,8,0] (indexesToPickUp 3 6 [3,8,9,1,2,5,4,6,7])
testIndexesToPickUpAt7 = TestCase $ assertEqual "testIndexesToPickUp" [8,0,1] (indexesToPickUp 3 7 [3,8,9,1,2,5,4,6,7])
testIndexesToPickUpAt8 = TestCase $ assertEqual "testIndexesToPickUp" [0,1,2] (indexesToPickUp 3 8 [3,8,9,1,2,5,4,6,7])

testPickUpAt0 = TestCase $ assertEqual "pickUp" ([8,9,1], [3,2,5,4,6,7]) (pickUp 3 0 [3,8,9,1,2,5,4,6,7])
testPickUpAt1 = TestCase $ assertEqual "pickUp" ([8,9,1], [2,5,4,6,7,3]) (pickUp 3 1 [3,2,8,9,1,5,4,6,7])
testPickUpAt2 = TestCase $ assertEqual "pickUp" ([4,6,7], [5,8,9,1,3,2]) (pickUp 3 2 [3,2,5,4,6,7,8,9,1])
testPickUpAt3 = TestCase $ assertEqual "pickUp" ([9,1,3], [8,4,6,7,2,5]) (pickUp 3 3 [7,2,5,8,9,1,3,4,6])
testPickUpAt4 = TestCase $ assertEqual "pickUp" ([6,7,9], [4,1,3,2,5,8]) (pickUp 3 4 [3,2,5,8,4,6,7,9,1])
testPickUpAt5 = TestCase $ assertEqual "pickUp" ([3,6,7], [1,9,2,5,8,4]) (pickUp 3 5 [9,2,5,8,4,1,3,6,7])
testPickUpAt6 = TestCase $ assertEqual "pickUp" ([3,6,7], [9,2,5,8,4,1]) (pickUp 3 6 [7,2,5,8,4,1,9,3,6])
testPickUpAt7 = TestCase $ assertEqual "pickUp" ([5,8,3], [2,6,7,4,1,9]) (pickUp 3 7 [8,3,6,7,4,1,9,2,5])
testPickUpAt8 = TestCase $ assertEqual "pickUp" ([7,4,1], [6,5,8,3,9,2]) (pickUp 3 8 [7,4,1,5,8,3,9,2,6])

testSelectDestinationCup = TestCase $ assertEqual "selectDestinationCup" 2 (selectDestinationCup 3 [8,9,1] [3,2,5,4,6,7])
testSelectDestinationCupIncludedInPickedUpCups = TestCase $ assertEqual "selectDestinationCupIncludedInPickedUpCups" 7 (selectDestinationCup 8 [8,9,1] [3,2,5,4,6,7])
testSelectDestinationCupIncludedInPickedUpCupsWithWrap = TestCase $ assertEqual "testSelectDestinationCupIncludedInPickedUpCupsWithWrap" 7 (selectDestinationCup 2 [8,9,1] [3,2,5,4,6,7])

tests = hUnitTestToTests $ TestList [
  TestLabel "testPlayGameFor1Rounds" testPlayGameFor1Rounds,
  TestLabel "testPlayGameFor2Rounds" testPlayGameFor2Rounds,
  TestLabel "testPlayGameFor3Rounds" testPlayGameFor3Rounds,
  TestLabel "testPlayGameFor4Rounds" testPlayGameFor4Rounds,
  TestLabel "testPlayGameFor5Rounds" testPlayGameFor5Rounds,
  TestLabel "testPlayGameFor6Rounds" testPlayGameFor6Rounds,
  TestLabel "testPlayGameFor7Rounds" testPlayGameFor7Rounds,
  TestLabel "testPlayGameFor8Rounds" testPlayGameFor8Rounds,
  TestLabel "testPlayGameFor9Rounds" testPlayGameFor9Rounds,
  TestLabel "testPlayGameFor10Rounds" testPlayGameFor10Rounds,
  TestLabel "testPlayGameFor100Rounds" testPlayGameFor100Rounds,
  TestLabel "testIndexesToPickUpAt0" testIndexesToPickUpAt0,
  TestLabel "testIndexesToPickUpAt5" testIndexesToPickUpAt5,
  TestLabel "testIndexesToPickUpAt6" testIndexesToPickUpAt6,
  TestLabel "testIndexesToPickUpAt7" testIndexesToPickUpAt7,
  TestLabel "testIndexesToPickUpAt8" testIndexesToPickUpAt8,
  TestLabel "testPickUpAt0" testPickUpAt0,
  TestLabel "testPickUpAt1" testPickUpAt1,
  TestLabel "testPickUpAt2" testPickUpAt2,
  TestLabel "testPickUpAt3" testPickUpAt3,
  TestLabel "testPickUpAt4" testPickUpAt4,
  TestLabel "testPickUpAt5" testPickUpAt5,
  TestLabel "testPickUpAt6" testPickUpAt6,
  TestLabel "testPickUpAt7" testPickUpAt7,
  TestLabel "testPickUpAt8" testPickUpAt8,
  TestLabel "testSelectDestinationCup" testSelectDestinationCup,
  TestLabel "testSelectDestinationCupIncludedInPickedUpCups" testSelectDestinationCupIncludedInPickedUpCups,
  TestLabel "testSelectDestinationCupIncludedInPickedUpCupsWithWrap" testSelectDestinationCupIncludedInPickedUpCupsWithWrap

  ]

main = defaultMain tests
