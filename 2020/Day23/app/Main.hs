{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Day2 (playGame2)
import Circle
import Data.IORef

createLargeList :: [Int]
createLargeList = initialNumbers ++ [10..1000000]
  where
    initialNumbers = [7, 9, 2, 8, 4, 5, 1, 3, 6]

main :: IO ()
main = do
    let input = createLargeList
    maybeIndex <- playGame2 10000000 input
    case maybeIndex of
      Just index -> do
        let maybeNode1 = findNode 1 index
        case maybeNode1 of
          Just node1 -> do
            putStrLn $ "node1: " ++ (show (value node1))
            maybeNode2 <- readIORef (next node1)
            case maybeNode2 of
              Just node2 -> do
                putStrLn $ "node2: " ++ (show (value node2))
                maybeNode3 <- readIORef (next node2)
                case maybeNode3 of
                  Just node3 -> do
                    putStrLn $ "node3: " ++ (show (value node3))
                    putStrLn $ "result: " ++ (show ((value node2) * (value node3)))
                  Nothing -> error "Could not find next node"
              Nothing -> error "Could not find next node"
          Nothing -> error "Could not find node 1"
      Nothing -> error "Did not get an index"

