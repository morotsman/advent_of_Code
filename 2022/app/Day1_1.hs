module Main where

import Data.List.Split (splitWhen)
import Data.List (maximumBy)

-- Function to convert a list of strings to a list of integers
stringListToIntList :: [String] -> [Int]
stringListToIntList = map read

-- Function to sum each inner list and return a list of sums
sumEachList :: [[String]] -> [Int]
sumEachList = map (sum . stringListToIntList)

main :: IO ()
main = do
  content <- readFile "/Users/nikleo/workspace/advent/2022/app/Day1_input.txt"
  let linesOfFiles = lines content
  let result = (maximum . sumEachList . splitWhen (== "")) (lines content)
  print result
