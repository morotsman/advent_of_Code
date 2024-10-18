{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Challenge1



main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day24/app/example.txt"
  let linesOfFile = lines content

  let tileVisits = visitedTiles linesOfFile
  let allBlackTiles = blackTiles tileVisits
  putStrLn $ show (length allBlackTiles)

