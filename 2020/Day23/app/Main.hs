{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Day (playGame)

main :: IO ()
main = do
  print ("Part1: " ++ show (playGame 100 792845136))
