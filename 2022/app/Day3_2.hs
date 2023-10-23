module Main where

import Data.Set (fromList, toList, intersection)
import Data.Char (ord, isUpper)
import Data.List.Split (chunksOf)

intersectGroups :: [[String]] -> [Char]
intersectGroups groups = concatMap intersect groups

intersect :: [String] -> [Char]
intersect rows = foldl (\acc x -> toList (intersection (fromList acc) (fromList x))) (['a'..'z'] ++ ['A'..'Z']) rows

scoreCharacters :: [Char] -> Int
scoreCharacters cs = sum (map charToPoint cs)

charToPoint :: Char -> Int
charToPoint c | isUpper(c) = (ord c) - (ord 'A') + 27
              | otherwise = (ord c) - (ord 'a') + 1

main :: IO ()
main = do
  content <- readFile "/Users/nikleo/workspace/advent/2022/app/Day3_input.txt"
  let linesOfFile = lines content
  let result = (scoreCharacters . intersectGroups . chunksOf 3) linesOfFile
  print result
