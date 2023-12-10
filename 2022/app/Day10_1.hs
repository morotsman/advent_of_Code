{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens

type Answer = Maybe Int

type RegistryValue = Int
type Cycle = Int

data Instruction = Noop
                  | AddX Int
                  deriving (Show, Eq)

solveIt :: [String] -> Answer
solveIt input = do
  instructions <- traverse parseRow input
  let interestingColumns = [20, 60, 100, 140, 180, 220]
  --let interestingColumns = [1, 2, 3, 4, 5, 6, 7]
  let allResults = execute instructions (1, 0)
  let interestingResults = filter (\ (_, cycle) ->  elem cycle interestingColumns) allResults
  let theSum = sum $ map (\ (value, cycle) -> value * cycle) interestingResults
  return theSum

parseRow :: String -> Maybe (Instruction)
parseRow input | stringBeginningWith "noop" input = Just Noop
               | otherwise = fmap (\d -> AddX d) (getValue input)

stringBeginningWith :: String -> String -> Bool
stringBeginningWith beginning string = beginning == take (length beginning) string

getValue :: String -> Maybe Int
getValue row = fmap read (words row) ^? element 1

execute :: [Instruction] -> (RegistryValue, Cycle) -> [(RegistryValue, Cycle)]
execute [] _ = []
execute (i:is) (currentValue, currentCycle) | i == Noop =
                                       let cycle = currentCycle + 1
                                       in [(currentValue, cycle)] ++ execute is (currentValue, cycle)
                                     | otherwise =
                                       let (AddX value) = i
                                           updatedValue = currentValue + value
                                       in [ (currentValue, currentCycle + 1), (currentValue, currentCycle + 2)] ++ execute is (updatedValue, currentCycle + 2)

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day10_input.txt"
  let linesOfFile = lines content
  let answer = solveIt linesOfFile
  print answer
