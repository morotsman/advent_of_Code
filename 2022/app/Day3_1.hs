module Main where

import Data.Set (fromList, toList, intersection)
import Data.Char (ord, isUpper)

splitInHalf :: String -> (String, String)
splitInHalf str = splitAt (length str `div` 2) str

intersect :: (String, String) -> [Char]
intersect (s1, s2) = toList (intersection (fromList s1) (fromList s2))

scoreCharacters :: [Char] -> Int
scoreCharacters cs = sum (map charToPoint cs)

charToPoint :: Char -> Int
charToPoint c | isUpper(c) = (ord c) - (ord 'A') + 27
              | otherwise = (ord c) - (ord 'a') + 1

main :: IO ()
main = do
  content <- readFile "/Users/nikleo/workspace/advent/2022/app/Day3_input.txt"
  let linesOfFile = lines content
  let result = sum (map (scoreCharacters . intersect . splitInHalf) linesOfFile)
  print result
