{-# LANGUAGE ScopedTypeVariables #-}
module Day211 (countNonSuspects, findNonSuspects, findAllAllergensInSuspects, findAllAllergens, findAllSuspects, findSuspects, parseRows, toFood, parseFood, Food(..)) where

import Data.List (elemIndex)
import Data.Maybe (isJust, fromJust)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Debug.Trace (trace)

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
  allergens = Set.fromList $ fmap removeCommaAtEnd $ drop 1 $ words $ tail $ init allergensString
  in Food ingredients allergens

removeCommaAtEnd :: String -> String
removeCommaAtEnd s = if last s == ','
  then init s
  else s

findAllSuspects :: [Food] -> Map.Map String (Set.Set String)
findAllSuspects = foldl (\acc food -> Map.unionWith Set.intersection acc (findSuspects food)) Map.empty

findSuspects :: Food -> Map.Map String (Set.Set String)
findSuspects (Food ingredients allergens) =
  foldl (\acc allergen -> Map.insert allergen ingredients acc) Map.empty allergens

findAllAllergens :: [Food] -> Set.Set String
findAllAllergens foods = foldl (\acc food -> Set.union acc (allergens food)) Set.empty foods

findAllAllergensInSuspects :: Map.Map String (Set.Set String) -> Set.Set String
findAllAllergensInSuspects suspects = foldl (\acc suspect -> Set.union acc suspect) Set.empty (Map.elems suspects)

findNonSuspects :: [Food] -> Set.Set String
findNonSuspects foods = let
  allIngredients = foldl (\acc food -> Set.union acc (ingredients food)) Set.empty foods
  allSuspects = findAllAllergensInSuspects $ findAllSuspects foods
  in Set.difference allIngredients allSuspects

countNonSuspects :: [Food] -> Int
countNonSuspects foods = let
  nonSuspects = findNonSuspects foods
  in
    trace ("ljdfsljdfsljdsfljdfslkjdsfljsdflkjdfs")
    foldl (\acc food -> acc + (length $ Set.intersection nonSuspects (ingredients food))) 0 foods
