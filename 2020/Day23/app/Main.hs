{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Day (playGame)
import Day2 (playGame2)

main :: IO ()
main = do
  print ("Part1: " ++ show (playGame 100 792845136))

  --let input = [3, 8, 9, 1, 2, 5, 4, 6, 7]
  let input = []
  --let rest = take (1000000 - length input) [10..]
  let rest = take 1000 [10..]
  --print ("test: " ++ show(length (input ++ rest)))
  let part2 = playGame2 250 (input ++ rest)
  --let result = take 3 $ dropWhile (\l -> not (l == 1)) part2
  print ("Part2: " ++ show part2)
  --print ("Part2: " ++ show (playGame2 100 [3, 8, 9, 1, 2, 5, 4, 6, 7]))
  --print ("Part2: " ++ show (playGame2 100 [7, 9, 2, 8, 4, 5, 1, 3, 6]))

