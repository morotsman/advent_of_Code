{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Day (playGame)
-- import Day2 (runArrayExample)

import Data.Array.IO
import Control.Monad


-- Initialize the cup array
initializeCups :: [Int] -> IO (IOArray Int Int)
initializeCups cups = do
    let maxCup = maximum cups
    arr <- newArray (1, maxCup) 0 :: IO (IOArray Int Int)
    zipWithM_ (writeArray arr) cups (tail (cycle cups))
    return arr

-- Perform 100 moves on the cup array
makeMoves :: Int -> IOArray Int Int -> Int -> Int -> IO ()
makeMoves 0 _ _ _ = return ()
makeMoves moves cups currentCup maxCup = do
    -- Pick up the next three cups
    cup1 <- readArray cups currentCup
    cup2 <- readArray cups cup1
    cup3 <- readArray cups cup2
    nextCup <- readArray cups cup3

    -- Find the destination cup
    let destination = findDestination (currentCup - 1) [cup1, cup2, cup3] maxCup

    -- Link current cup to the cup after the three picked up
    writeArray cups currentCup nextCup

    -- Link the picked-up cups after the destination cup
    afterDest <- readArray cups destination
    writeArray cups destination cup1
    writeArray cups cup3 afterDest

    -- Move to the next current cup
    makeMoves (moves - 1) cups nextCup maxCup

-- Helper to find the destination cup, skipping the picked-up cups
findDestination :: Int -> [Int] -> Int -> Int
findDestination dest pickedUp maxCup
    | dest < 1 = findDestination maxCup pickedUp maxCup
    | dest `elem` pickedUp = findDestination (dest - 1) pickedUp maxCup
    | otherwise = dest

-- Get the final cup sequence after 100 moves
getCupLabels :: IOArray Int Int -> IO [Int]
getCupLabels cups = do
    cup1 <- readArray cups 1
    getSequence cups cup1

getSequence :: IOArray Int Int -> Int -> IO [Int]
getSequence cups start = go start []
  where
    go 1 acc = return (reverse acc)
    go x acc = do
        next <- readArray cups x
        go next (x : acc)



main :: IO ()
main = do
    let inputCups = [7, 9, 2, 8, 4, 5, 1, 3, 6]
    cups <- initializeCups inputCups
    makeMoves 100 cups (head inputCups) (maximum inputCups)
    result <- getCupLabels cups
    putStrLn $ concatMap show result

