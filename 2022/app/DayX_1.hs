{-# LANGUAGE ScopedTypeVariables #-}
module Main where

type Answer = [String]

solveIt :: [String] -> Answer
solveIt input = input

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day8_example.txt"
  let linesOfFile = lines content
  let answer = solveIt linesOfFile
  print answer
