{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.List.Split (splitWhen)
import Data.Set (Set, fromList, toList, intersection)

toRanges :: [String] -> [[Set Int]]
toRanges lines = do
  let elfPairs :: [[String]] = toElfPairAssignments lines
  map toIntSets elfPairs

toElfPairAssignments :: [String] -> [[String]]
toElfPairAssignments rows = map (splitWhen (== ',')) rows

-- ["2-4","6-8"] -> [[2,3,4], [6,7,8]]
toIntSets :: [String] -> [Set Int]
toIntSets ranges = map toIntSet ranges

-- "2-4" -> [2,3,4]
toIntSet :: String ->  Set Int
toIntSet range = listToRange (map (\d -> read d) (splitWhen (== '-') range))

listToRange :: [Int] -> Set Int
listToRange [start, end] = fromList [start..end]
listToRange _ = fromList []

isTotallyOverLapping :: [Set Int] -> Bool
isTotallyOverLapping [set1, set2] = do
  let overlap = intersection set1 set2
  overlap == set1 || overlap == set2

main :: IO ()
main = do
  content <- readFile "/Users/nikleo/workspace/advent/2022/app/Day4_input.txt"
  let linesOfFile = lines content
  let rangePairs :: [[Set Int]] = toRanges linesOfFile
  let result = filter isTotallyOverLapping rangePairs
  print (length result)
