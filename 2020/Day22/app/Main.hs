{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Day (scoreRecursiveGame, RecursivePlayer(..), getRecursivePlayers, playGameRecursiveCombat, scoreGame, playGame, getPlayers)

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day22/app/input.txt"
  let linesOfFile = lines content
  print ("Part1: " ++ show (scoreGame $ playGame $ getPlayers linesOfFile))
  let (player1, player2) = getRecursivePlayers linesOfFile
  print ("Part2: " ++ show (scoreRecursiveGame $ playGameRecursiveCombat player1 player2))
