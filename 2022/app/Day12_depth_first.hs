{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import MatrixUtil
import Debug.Trace
import Data.List (minimumBy)
import Data.Ord (comparing)
import Data.Set (Set)
import qualified Data.Set as Set

type Path = [Coordinate]

solveIt :: Matrix Char -> Maybe Path
solveIt matrix = let
  maybeStartPosition = findElement matrix (\e -> e == 'S')
  in fmap (traverseMatrix matrix) maybeStartPosition

traverseMatrix :: Matrix Char -> Coordinate -> Path
traverseMatrix matrix startPosition = let
  paths = traverseMatrix' matrix startPosition []
  in minimumBy (comparing length) $ filter (\path -> length path > 0) paths

traverseMatrix' :: Matrix Char -> Coordinate -> Path -> [Path]
traverseMatrix' matrix coordinate@(column, row) traveledPath | elementAt matrix column row == Just 'E' =
  trace "Found it!!!"
  [coordinate : traveledPath]
traverseMatrix' matrix coordinate@(column, row) traveledPath = let
  allNeighbours = neighbours matrix coordinate
  visitedCoordinates = Set.fromList traveledPath
  unvisitedNeighbours = filter (\c -> not (Set.member c visitedCoordinates)) allNeighbours
  maybeCurrentHeight = fmap (\h -> if h == 'S' then 'a' else h) $ elementAt matrix column row
  validNeighbours = case maybeCurrentHeight of
    Just currentHeight -> filter (isValidNeighbour matrix currentHeight) unvisitedNeighbours
    Nothing -> []
  in case validNeighbours of
    [] ->
      trace "nope"
      []
    ns ->
      trace "searching"
      trace (show traveledPath)
      foldr (\n acc ->  (traverseMatrix' matrix n (coordinate:traveledPath)) ++ acc) ([] :: [Path]) ns


isValidNeighbour :: Matrix Char -> Char -> Coordinate -> Bool
isValidNeighbour matrix currentHeight (column, row) =
  case elementAt matrix column row of
    Just neighbourHeight -> abs (fromEnum (if neighbourHeight == 'E' then 'z' else neighbourHeight) - fromEnum currentHeight) <= 1
    Nothing -> False

neighbours :: Matrix Char -> Coordinate -> [Coordinate]
neighbours matrix (column, row) =
  let possibleCoordinates = [(column - 1, row), (column + 1, row), (column, row - 1), (column, row + 1)]
  in filter (\(column, row) -> not $ outOfBoundary column row matrix) possibleCoordinates

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day12_example_1.txt"
  let linesOfFile :: Matrix Char = Matrix (lines content)
  let answer = solveIt linesOfFile
  print answer
  print (fmap (\a -> length a) answer)