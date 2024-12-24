{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List.Split (splitOn)
import Data.List (sortBy, elemIndex)
import Debug.Trace
import Text.Parsec
import Text.Parsec.String (Parser)

data Package
  = Single Int         -- Represents a single integer
  | List [Package]     -- Represents a nested list of Packages
  deriving (Show, Eq)

type Answer = Either ParseError (Maybe Int)

divider2 = List [List [Single 2]]
divider6 = List [List [Single 6]]

solveIt :: [String] -> Answer
solveIt = fmap (solveIt' . sortedPackages . concat) . parseInput

solveIt' :: [Package] -> Maybe Int
solveIt' packages = do
  indexDivider2 :: Int <- elemIndex divider2 packages
  indexDivider6 :: Int <- elemIndex divider6 packages
  return ((indexDivider2 + 1) * (indexDivider6 + 1))

maybeToEither :: l -> Maybe r -> Either l r
maybeToEither leftValue = maybe (Left leftValue) Right

sortedPackages :: [Package] -> [Package]
sortedPackages = sortBy orderPair

orderPair :: Package -> Package -> Ordering
orderPair (Single v1) (Single v2)
  | v1 == v2  = EQ
  | v1 < v2   = LT
  | otherwise = GT
orderPair (Single v1) (List list2) = orderPair (List [Single v1]) (List list2)
orderPair (List list1) (Single v2) = orderPair (List list1) (List [Single v2])
orderPair (List []) (List (_:ps2)) = LT
orderPair (List (_:ps1)) (List []) = GT
orderPair (List []) (List [])      = EQ
orderPair (List (p1:ps1)) (List (p2:ps2)) =
  let result = orderPair p1 p2
  in if result == EQ then orderPair (List ps1) (List ps2) else result

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
