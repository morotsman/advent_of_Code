{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Day211 (countNonSuspects, findAllSuspects, parseRows, toFood, Food(..))

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day21/app/Day21_input.txt"
  let linesOfFile = lines content
  print (show (findAllSuspects (parseRows linesOfFile)))
