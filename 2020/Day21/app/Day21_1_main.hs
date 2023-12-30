{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Day211 (parseRows, toFood, Food(..))

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day21/app/Day21_example.txt"
  let linesOfFile = lines content
  print (show (parseRows linesOfFile))
