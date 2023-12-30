{-# LANGUAGE ScopedTypeVariables #-}
module Day211 (parseRows, toFood, parseFood, Food(..)) where

import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)

type Answer = [String]

solveIt :: [String] -> Answer
solveIt input = input

data Food = Food {
  ingredients :: [String],
  allergens :: [String]
} deriving (Show, Eq)

parseRows :: [String] -> [Food]
parseRows rows = foldl (\acc x -> acc ++ [parseFood x]) [] rows

parseFood :: String -> Food
parseFood s = let
  maybeIndex = elemIndex '(' s
  in if isJust maybeIndex
    then toFood(splitAt (fromJust maybeIndex) s)
    else Food (words s) []

toFood :: ([Char], [Char]) -> Food
toFood (ingredientsString, allergensString) = let
  ingredients = words ingredientsString
  allergens = drop 1 $ words $ tail $ init allergensString
  in Food ingredients allergens
