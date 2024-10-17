module Main where

import Challenge1

import qualified Data.Map as Map


main :: IO ()
main = do

  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day24/app/input.txt"
  let linesOfFile = lines content

  let allDiscoveredTiles = map (\line -> head $ traversePath (0, 0) (getPath line)) linesOfFile

  mapM_ (putStrLn . show) allDiscoveredTiles


  let tileVisits = countOccurrences allDiscoveredTiles

  mapM_ (\(tile, count) -> putStrLn $ show tile ++ " visited " ++ show count ++ " times") (Map.toList tileVisits)

  let blackTiles = filter (\(_, count) -> odd count) (Map.toList tileVisits)

  putStrLn $ show (length blackTiles)

countOccurrences :: (Ord k) => [k] -> Map.Map k Int
countOccurrences = foldr (\key acc -> Map.insertWith (+) key 1 acc) Map.empty

