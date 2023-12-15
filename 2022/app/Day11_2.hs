{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Data.List (sort)
import Day11_example_1 (Monkey(..), getExampleMonkeys, getInputMonkeys)

type Answer = [[Monkey]]

solveIt :: [Monkey] -> Answer
solveIt = takeTurns 10000

takeTurns :: Int -> [Monkey] -> [[Monkey]]
takeTurns _ [] = []
takeTurns 0 (m:ms) = []
takeTurns turn monkeys = let
  newMonkeys = takeTurn monkeys
  in [newMonkeys] ++ takeTurns (turn-1) newMonkeys

takeTurn :: [Monkey] -> [Monkey]
takeTurn monkeys = foldl takeTurnForMonkey monkeys [0..(length monkeys - 1)]

takeTurnForMonkey :: [Monkey] -> Int -> [Monkey]
takeTurnForMonkey monkeys monkeyIndex | null (startingItems $ monkeys !! monkeyIndex)  = monkeys
                             | otherwise = let
  monkey = monkeys !! monkeyIndex
  (i: is) = startingItems monkey
  newWorryLevel = (operation monkey i) `mod` 9699690
  toMonkeyIndex = if (test monkey newWorryLevel) then throwToIfTrue monkey else throwToIfFalse monkey
  newFromMonkey = monkey { startingItems = is, inspections = (inspections monkey) + 1 }
  oldToMonkey = monkeys !! toMonkeyIndex
  newToMonkey = oldToMonkey { startingItems = newWorryLevel : startingItems oldToMonkey }
  newMonkeys = replaceMonkey (replaceMonkey monkeys newToMonkey) newFromMonkey
  in takeTurnForMonkey newMonkeys monkeyIndex

replaceMonkey :: [Monkey] -> Monkey -> [Monkey]
replaceMonkey monkeys monkey = let
  index = monkeyIndex monkey
  (before, _: after) = splitAt index monkeys
  in before ++ [monkey] ++ after

showMonkeyTurn :: Monkey -> Int
showMonkeyTurn monkey = (inspections monkey)

main :: IO ()
main = do
  let monkeys :: [Monkey] = getInputMonkeys
  let turns :: [[Monkey]] = solveIt monkeys
  let results = fmap (fmap showMonkeyTurn) turns
  print (reverse $ sort $ last results)
