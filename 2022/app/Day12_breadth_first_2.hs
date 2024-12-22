{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import MatrixUtil
import Debug.Trace
import Data.List (minimumBy, groupBy)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

type Path = [Coordinate]

solveIt :: Matrix Char -> Maybe Path
solveIt matrix = let
  maybeStartPosition = findElement matrix (\e -> e == 'S')
  in fmap (traverseTopology matrix) maybeStartPosition

traverseTopology :: Matrix Char -> Coordinate -> Path
traverseTopology matrix startPosition = traverseTopology' matrix (Set.singleton startPosition) [[startPosition]]

traverseTopology' :: Matrix Char -> Set Coordinate -> [Path] -> Path
traverseTopology' topology visited paths = let
  newValidPaths = removeDuplicates $ concatMap (validPaths topology visited) paths
  newVisited = Set.union visited $ Set.fromList (fmap head paths)
  maybeGoal = filter (foundGoal topology) newValidPaths
  in
    if null newValidPaths then
      []
    else if (null maybeGoal) then
      --trace ("Level: " ++ show (length $ head newValidPaths))
      --trace (show $ length newValidPaths)
      --trace (show newValidPaths)
      --trace (show $ head (fmap (\path -> fmap (\(column, row) -> elementAt topology column row) path) newValidPaths))
      traverseTopology' topology newVisited newValidPaths
    else (head maybeGoal)


removeDuplicates :: [Path] -> [Path]
removeDuplicates paths =
  Map.elems $ Map.fromListWith (\_ p -> p) [(head path, path) | path <- paths]


foundGoal :: Matrix Char -> Path -> Bool
foundGoal topology path = let
    (column, row) = head path
    maybePosition = elementAt topology column row
  in case maybePosition of
    Just 'E' -> True
    _ -> False


validPaths :: Matrix Char -> Set Coordinate -> Path -> [Path]
validPaths topology visited path = let
    currentHead = head path
    unvisitedNeighbours = filter (\c -> not (Set.member c visited)) $ validNeighbours topology currentHead
  in
    fmap (\neighbour -> neighbour : path) unvisitedNeighbours

validNeighbours :: Matrix Char -> Coordinate -> [Coordinate]
validNeighbours topology coordinate@(column, row) = let
  allNeighbours = neighbours topology coordinate
  maybeCurrentHeight = fmap (\h -> if h == 'S' then 'a' else h) $ elementAt topology column row
  validNeighbours = case maybeCurrentHeight of
    Just currentHeight -> filter (isValidNeighbour topology currentHeight) allNeighbours
    Nothing -> []
  in validNeighbours


neighbours :: Matrix Char -> Coordinate -> [Coordinate]
neighbours topology (column, row) =
  let possibleCoordinates = [(column - 1, row), (column + 1, row), (column, row - 1), (column, row + 1)]
  in filter (\(column, row) -> not $ outOfBoundary column row topology) possibleCoordinates

isValidNeighbour :: Matrix Char -> Char -> Coordinate -> Bool
isValidNeighbour topology currentHeight (column, row) =
  case elementAt topology column row of
    Just neighbourHeight ->
      if (fromEnum (if neighbourHeight == 'E' then 'z' else neighbourHeight) < fromEnum currentHeight) then
        True
      else
        abs (fromEnum (if neighbourHeight == 'E' then 'z' else neighbourHeight) - fromEnum currentHeight) <= 1
    Nothing -> False

main :: IO ()
main = do
  --content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day12_example_1.txt"
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day12_input.txt"
  let linesOfFile :: Matrix Char = Matrix (lines content)
  let answer = solveIt linesOfFile
  print answer
  print (fmap (\a -> length a) answer)