{-# LANGUAGE ScopedTypeVariables #-}
module Challenge (parseRules, possibleMatches) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.List (intercalate)

data Rule
  = AND [Int]
  | Literal Char
  | OR [Rule]
  deriving (Show, Eq)

possibleMatches :: Map.Map Int Rule -> Int -> [String]
possibleMatches allRules ruleNum =
  case Map.lookup ruleNum allRules of
    Just rule -> case rule of
      Literal c -> [[c]]
      OR rs -> concatMap (possibleMatchesForRule allRules) rs
      AND rs -> foldr (\r acc -> let
        matches = possibleMatches allRules r
        in [x ++ y | x <- matches, y <- acc]) [""] rs
    Nothing -> []

possibleMatchesForRule :: Map.Map Int Rule -> Rule -> [String]
possibleMatchesForRule allRules (Literal c) = [[c]]
possibleMatchesForRule allRules (AND rs) = foldr (\r acc -> let
    matches = possibleMatches allRules r
    in [x ++ y | x <- matches, y <- acc]) [""] rs
possibleMatchesForRule _ _ = []

parseRules :: [String] -> Map.Map Int Rule
parseRules lines = Map.fromList $ map parseRuleLine lines

parseRuleLine :: String -> (Int, Rule)
parseRuleLine line =
  let (ruleNumStr, ruleDef) = span (/= ':') line
      ruleNum = read ruleNumStr
  in (ruleNum, parseRule (drop 2 ruleDef))

parseRule :: String -> Rule
parseRule str
  | '"' `elem` str = Literal (head $ filter (/= '"') str)
  | '|' `elem` str = OR (map (AND . map read . words) (splitOn "|" str))
  | otherwise = AND (map read (words str))

splitOn :: String -> String -> [String]
splitOn _ "" = []
splitOn delim str =
  case breakSubstring delim str of
    (before, "") -> [before]
    (before, after) -> before : splitOn delim (drop (length delim) after)

breakSubstring :: String -> String -> (String, String)
breakSubstring delim str =
  case findSubstring delim str of
    Nothing -> (str, "")
    Just idx -> splitAt idx str

findSubstring :: String -> String -> Maybe Int
findSubstring delim str
  | delim `isPrefixOf` str = Just 0
  | null str = Nothing
  | otherwise = (+1) <$> findSubstring delim (tail str)
