{-# LANGUAGE ScopedTypeVariables #-}
module Day211 (parseRows, toFood, parseFood, Food(..)) where

import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as Set

type Answer = [String]

solveIt :: [String] -> Answer
solveIt input = input

data Food = Food {
  ingredients :: Set.Set String,
  allergens :: Set.Set String
} deriving (Show, Eq)

parseRows :: [String] -> [Food]
parseRows rows = foldl (\acc x -> acc ++ [parseFood x]) [] rows

parseFood :: String -> Food
parseFood s = let
  maybeIndex = elemIndex '(' s
  in if isJust maybeIndex
    then toFood(splitAt (fromJust maybeIndex) s)
    else Food (Set.fromList $ words s) Set.empty

toFood :: ([Char], [Char]) -> Food
toFood (ingredientsString, allergensString) = let
  ingredients = Set.fromList $ words ingredientsString
  allergens = Set.fromList $ drop 1 $ words $ tail $ init allergensString
  in Food ingredients allergens
