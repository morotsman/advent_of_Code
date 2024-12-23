{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List.Split (splitOn)
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

data Package
  = Single Int         -- Represents a single integer
  | List [Package]     -- Represents a nested list of Packages
  deriving (Show, Eq)

data Result = LeftGreater | RightGreater | Equal deriving (Show, Eq)

type Answer = Either ParseError Int

solveIt :: [String] -> Answer
solveIt = fmap solveIt' . parseInput

solveIt' :: [[Package]] -> Int
solveIt' = sum . fmap fst . filter snd . zip [1  :: Int ..] . fmap checkPair

checkPair :: [Package] -> Bool
checkPair pair = checkPair' pair == LeftGreater


checkPair' :: [Package] -> Result
checkPair' [Single v1, Single v2] | v1 == v2 = Equal
                                  | v1 < v2  = LeftGreater
                                  | v1 > v2  = RightGreater
checkPair' [Single v1, list2]                = checkPair' [List [Single v1], list2]
checkPair' [list1, Single v2]                = checkPair' [list1 , List [Single v2]]
checkPair' [List [], List (p2:ps2)]          = LeftGreater
checkPair' [List (p1:ps1), List []]          = RightGreater
checkPair' [List [], List []]                = Equal
checkPair' [List (p1:ps1), List (p2:ps2)]    = let
  result = checkPair' [p1, p2]
  in if (result == Equal) then checkPair' [List ps1, List ps2] else result

parseInput :: [String] -> Either ParseError [[Package]]
parseInput = traverse id . fmap parsePackages . groupOnEmpty

groupOnEmpty :: [String] -> [[String]]
groupOnEmpty = filter (not . null) . splitOn [""]

parsePackages :: [String] -> Either ParseError [Package]
parsePackages = traverse parsePackage

parsePackage :: String -> Either ParseError Package
parsePackage = parse lineParser ""

lineParser :: Parser Package
lineParser = packageParser <* spaces <* eof

packageParser :: Parser Package
packageParser = singleParser <|> listParser

singleParser :: Parser Package
singleParser = Single <$> read <$> many1 digit

listParser :: Parser Package
listParser = do
  char '['
  packages <- packageParser `sepBy` char ','
  char ']'
  return $ List packages

main :: IO ()
main = do
  --content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day13_example.txt"
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day13_input.txt"
  let answer = solveIt $ lines content
  print answer
