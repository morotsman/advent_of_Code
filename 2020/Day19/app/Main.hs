{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Challenge
import qualified Data.Set as Set


main :: IO ()
main = do
  rulesInFile <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day19/app/rules2_example.txt"
  messagesInFile <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day19/app/example_input.txt"

  let rules = parseRules (lines rulesInFile)

  --0: 8 11
  --8: 42 | 42 8
  --11: 42 31 | 42 11 31

  --0: 8 11
  --8: 42 42 42 ….
  --11:
  --42 31
  --Or
  --42 42 31 31
  --Or
  --42 42 42 31 31 31

  let allPossibleMatchesFor42 = Set.fromList (possibleMatches rules 42)
  let allPossibleMatchesFor31 = Set.fromList (possibleMatches rules 31)

  putStrLn $ show allPossibleMatchesFor42
  putStrLn ""
  putStrLn $ show allPossibleMatchesFor31

  --let numberOfMatchingMessages = filter (\message -> Set.member message allPossibleMatches) (lines messagesInFile)

  --putStrLn $ show (length numberOfMatchingMessages)
