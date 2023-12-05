{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (sort)

type Answer = Int

type Tree = Int
type Forest = [[Tree]]
type Coordinate = (Int, Int)

solveIt :: [String] -> Answer
solveIt input = do
  let forest = toForest input
  let scores = scenicScores forest
  head $ reverse $ sort scores

toForest :: [String] -> Forest
toForest input = fmap (fmap (\x -> read [x] :: Int)) input

scenicScores :: Forest -> [Int]
scenicScores forest = do
  let coordinates = [(x,y) | x <- [0..length forest - 1], y <- [0..length (forest !! 0) - 1]]
  let visibleTrees = map (scenicScore forest) coordinates
  visibleTrees

scenicScore :: Forest -> Coordinate -> Int
scenicScore forest (x,y) = do
  let tree = getTree forest (x,y)
  countTreesShorterOrEqual (lookUp forest (x-1,y)) tree * countTreesShorterOrEqual (lookDown forest (x+1,y)) tree * countTreesShorterOrEqual (lookLeft forest (x,y-1)) tree * countTreesShorterOrEqual (lookRight forest (x,y+1)) tree

getTree :: Forest -> Coordinate -> Tree
getTree forest (x,y) = forest !! x !! y

countTreesShorterOrEqual :: [Tree] -> Tree -> Int
countTreesShorterOrEqual [] _ = 0
countTreesShorterOrEqual (x:xs) tree | x >= tree = 1
                                     | otherwise = 1 + countTreesShorterOrEqual xs tree
                                     
lookUp :: Forest -> Coordinate -> [Tree]
lookUp forest coordinate | outOfBounds forest coordinate = []
lookUp forest (x,y) = [getTree forest (x,y)] ++ lookUp forest (x-1, y)

outOfBounds :: Forest -> Coordinate -> Bool
outOfBounds forest (x,y) = x < 0 || y < 0 || x >= length forest || y >= length (forest !! 0)

lookDown :: Forest -> Coordinate -> [Tree]
lookDown forest coordinate | outOfBounds forest coordinate = []
lookDown forest (x,y) = [getTree forest (x,y)] ++ lookDown forest (x+1, y)

lookLeft :: Forest -> Coordinate -> [Tree]
lookLeft forest coordinate | outOfBounds forest coordinate = []
lookLeft forest (x,y) = [getTree forest (x,y)] ++ lookLeft forest (x, y-1)

lookRight :: Forest -> Coordinate -> [Tree]
lookRight forest coordinate | outOfBounds forest coordinate = []
lookRight forest (x,y) = [getTree forest (x,y)] ++ lookRight forest (x, y+1)

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day8_input.txt"
  let linesOfFile :: [[Char]] = lines content
  print (solveIt linesOfFile)
