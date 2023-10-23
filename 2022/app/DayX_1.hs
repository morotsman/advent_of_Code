module Main where

main :: IO ()
main = do
  content <- readFile "/Users/nikleo/workspace/advent/2022/app/Day3_example.txt"
  let linesOfFile = lines content
  print linesOfFile
