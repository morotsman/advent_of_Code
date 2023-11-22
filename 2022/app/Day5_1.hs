{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import qualified Data.Map as Map

solveIt :: [String] -> Map.Map String [Char] -> Maybe(Map.Map String [Char])
solveIt [] stacks = Just(stacks)
solveIt (r:rs) stacks = do
    (number, fromStackNumber, toStackNumber) <- parseRow r
    from <- Map.lookup fromStackNumber stacks
    to <- Map.lookup toStackNumber stacks
    let (updatedFromStack, updatedToStack) = move number from to
    let updatedStacks = foldl (\acc (k, v) -> Map.insert k v acc) stacks [(fromStackNumber, updatedFromStack), (toStackNumber, updatedToStack)]
    result <- solveIt rs updatedStacks
    return result

parseRow :: String -> Maybe(Int, String, String)
parseRow row = do
  let columns = words row
  numberToMove <- fmap read columns ^? element 1
  from <- columns ^? element 3
  to <- columns ^? element 5
  return (numberToMove, from, to)

move :: Int -> [Char] -> [Char] -> ([Char], [Char])
move 0 fs ts = (fs, ts)
move number [] ts = ([], ts)
move number (f:fs) ts = move (number-1) fs (f:ts)

topOfStacks :: Map.Map String [Char] -> [Char]
topOfStacks stacks = do
   map snd $ Map.toList $ fmap head stacks

main :: IO ()
main = do
  --let stacks = [("1", "NZ"),("2", "DCM"),("3", "P")]
  let stacks = [("1", "WLS"),("2", "QNTJ"),("3", "JFHCS"), ("4", "BGNWMRT"), ("5", "BQHDSLRT"), ("6", "LRHFVBJM"), ("7", "MJNRWD"), ("8", "JDNHFTZB"), ("9", "TFBNQLH")]
  let originalStacks = Map.fromList stacks

  content <- readFile "/Users/nikleo/workspace/advent_of_Code/2022/app/Day5_input.txt"
  let linesOfFile = lines content
  let result = fmap topOfStacks $ solveIt linesOfFile originalStacks
  print result

