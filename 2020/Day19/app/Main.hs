{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Challenge
import qualified Data.Set as Set

  --0: 8 11
  --8: 42 | 42 8
  --11: 42 31 | 42 11 31

  --0: 8 11
  --8: 42 42 42 etc
  --11:
  --42 31
  --Or
  --42 42 31 31
  --Or
  --42 42 42 31 31 31
  --etc

-- really ugly code to just match the rules
matchMessage :: Set.Set String -> Set.Set String -> String -> Bool
matchMessage possibleMatchesFor42 possibleMatchesFor31 "" = False
matchMessage possibleMatchesFor42 possibleMatchesFor31 message = go message 8
  where
    lettersToMatch = length $ Set.findMin possibleMatchesFor42 -- assumes all possible matches have the same number of letters

    go :: String -> Int -> Bool
    go "" 11 = True
    go msg ruleNum
      | ruleNum == 8 && matches42 msg = let
          newMessage = drop lettersToMatch msg
        in (if (length newMessage == 0) then False else go newMessage 8 || go newMessage 11)
      | ruleNum == 11 =
        if (matches42 msg && matches31 (takeEnd lettersToMatch msg)) then
          let newMessage = dropEnd lettersToMatch $ drop lettersToMatch msg
          in go newMessage 11
        else
          False
      | otherwise = False
      where
        matches42 = startMatches possibleMatchesFor42 lettersToMatch
        matches31 = startMatches possibleMatchesFor31 lettersToMatch

takeEnd :: Int -> String -> String
takeEnd n str = drop (length str - n) str

dropEnd :: Int -> String -> String
dropEnd n str = take (length str - n) str


startMatches :: Set.Set String -> Int -> String -> Bool
startMatches possibleMatches lettersToMatch message =
  Set.member (take lettersToMatch message) possibleMatches

main :: IO ()
main = do
  rulesInFile <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day19/app/rules2.txt"
  messagesInFile <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day19/app/input.txt"

  let rules = parseRules (lines rulesInFile)
  let messages = lines messagesInFile



  let allPossibleMatchesFor42 = Set.fromList (possibleMatches rules 42)
  let allPossibleMatchesFor31 = Set.fromList (possibleMatches rules 31)

  putStrLn $ show allPossibleMatchesFor42
  putStrLn ""
  putStrLn $ show allPossibleMatchesFor31
  putStrLn "answer:"
  putStrLn $ show $ length $ filter (matchMessage allPossibleMatchesFor42 allPossibleMatchesFor31) messages

  --let numberOfMatchingMessages = filter (\message -> Set.member message allPossibleMatches) (lines messagesInFile)






