{-# LANGUAGE ScopedTypeVariables #-}
module Day2 (playGame2) where

import qualified Data.Set as Set
import Debug.Trace (trace)
import Circle
import Data.IORef

type Cups = [Int]
type CurrentCup = Int
type Round = Int

maxInInput :: Int
maxInInput = 9

minInInput :: Int
minInInput = 1

inputLength :: Int
inputLength = 9

playGame2 :: Round -> [Int] -> IO (Maybe [Int])
playGame2 rounds cups = do
  (maybeHeadNode, index) <- fromList cups
  traverse (\headNode -> playGame' rounds headNode index) maybeHeadNode


playGame' :: Round -> Node Int -> NodeMap Int -> IO [Int]
playGame' 0 currentNode index = do
  toList currentNode
playGame' n currentNode index = do
  -- debug
  circleList <- toList currentNode
  putStrLn $ "Current node: " ++ show (value currentNode)
  putStrLn $ "cups: " ++ show circleList
  --
  (removedNodes, updatedIndexAfterRemove) <- removeAfter currentNode 3 index
  -- debug
  putStrLn $ "pick up: " ++ show removedNodes
  --
  let currentValue = value currentNode
  let destinationCup = selectDestinationCup currentValue updatedIndexAfterRemove
  -- debug
  putStrLn $ "destinationCup: " ++ show (value destinationCup)
  --
  updatedIndexAfterInsert <- insertListAfter destinationCup removedNodes index
  -- debug
  circleList <- toList currentNode
  putStrLn $ "cups2: " ++ show circleList
  --
  maybeNextNode <- readIORef (next currentNode)
  case maybeNextNode of
    Just nextNode ->
      playGame' (n-1) nextNode updatedIndexAfterInsert
    Nothing -> error "This should not be possible in a circle"


selectDestinationCup :: Int -> NodeMap Int -> Node Int
selectDestinationCup currentValue index | currentValue < minInInput = do
  selectDestinationCup maxInInput index
selectDestinationCup currentValue index = do
  let maybeNode :: Maybe (Node Int) = findNode (currentValue - 1) index
  case maybeNode of
    Just node -> node
    Nothing -> selectDestinationCup (currentValue - 1) index



