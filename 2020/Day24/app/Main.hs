module Main where

import Challenge1

import qualified Data.Map as Map


main :: IO ()
main = do
  putStrLn $ show $ getPath "nwnwneseeswswnenewneswwnewseswneseene"
  putStrLn $ show $ reverse $ traversePath (2,2) $ getPath "nwnwneseeswswnenewneswwnewseswneseene"
  putStrLn ""

  putStrLn $ show $ getPath "nenewswnwewswnenesenwnesewesw"
  putStrLn $ show $ reverse $ traversePath (4,4) $ getPath "nenewswnwewswnenesenwnesewesw"
  putStrLn ""

  putStrLn $ show $ getPath "eneswnwswnwsenenwnwnwwseeswneewsenese"
  putStrLn $ show $ reverse $ traversePath (2,2) $ getPath "eneswnwswnwsenenwnwnwwseeswneewsenese"
  putStrLn ""


  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/Day24/app/example.txt"
  let linesOfFile = lines content

  let allDiscoveredTiles = map (\line -> head $ traversePath (0, 0) (getPath line)) linesOfFile

  mapM_ (putStrLn . show) allDiscoveredTiles


  let tileVisits = countOccurrences allDiscoveredTiles

  mapM_ (\(tile, count) -> putStrLn $ show tile ++ " visited " ++ show count ++ " times") (Map.toList tileVisits)

countOccurrences :: (Ord k) => [k] -> Map.Map k Int
countOccurrences = foldr (\key acc -> Map.insertWith (+) key 1 acc) Map.empty

