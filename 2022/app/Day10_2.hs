{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Data.List.Split (chunksOf)

type Answer = Maybe [String]

type RegistryValue = Int
type Cycle = Int

data Instruction = Noop
                  | AddX Int
                  deriving (Show, Eq)

solveIt :: [String] -> Answer
solveIt input = do
  instructions <- traverse parseRow input
  let allResults = execute instructions (1, 0)
  return . map concat . chunksOf 40 $ map (\(value, cycle) -> if (isVisible (value, cycle)) then "#" else ".") allResults

isVisible :: (RegistryValue, Cycle) -> Bool
isVisible (value, cycle) = elem (mod (cycle-1) 40)  ([value-1, value, value + 1])

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
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day10_example_2.txt"
  print . solveIt . lines $ content
