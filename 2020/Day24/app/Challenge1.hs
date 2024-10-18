module Challenge1( Coordinate, Path, getPath, traversePath, visitedTiles, blackTiles) where

import qualified Data.Map as Map

-- column, row
type Column = Int
type Row = Int
-- offset coordinates as suggested here: https://math.stackexchange.com/questions/2254655/hexagon-grid-coordinate-system
type Coordinate = (Column, Row)

type Path = [String]

-- Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
-- Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.

flipTiles :: Map.Map Coordinate Int -> Map.Map Coordinate Int
flipTiles originalTiles = do
  undefined

colorOfAdjacentTiles :: Map.Map Coordinate Int -> Coordinate -> (Int, Int)
colorOfAdjacentTiles tiles tile = do
  let adjacentTiles = Map.elems $ adjacent tile
  undefined

blackTiles :: Map.Map Coordinate Int -> Map.Map Coordinate Int
blackTiles visitedTiles = Map.filter odd visitedTiles


visitedTiles :: [String] -> Map.Map Coordinate Int
visitedTiles lines = do
  let allDiscoveredTiles = map (\line -> head $ traversePath (0, 0) (getPath line)) lines
  countOccurrences allDiscoveredTiles


countOccurrences :: (Ord k) => [k] -> Map.Map k Int
countOccurrences = foldr (\key acc -> Map.insertWith (+) key 1 acc) Map.empty

-- e, se, sw, w, nw, and ne.
getPath :: String -> Path
getPath pathAsString =
  go pathAsString [] where
    go :: String -> [String] -> [String]
    go "" acc = reverse acc
    go path acc = do
      let maybeDirection = take 2 path
      case maybeDirection of
        "se" -> go (drop 2 path) ("se" : acc)
        "sw" -> go (drop 2 path) ("sw" : acc)
        "nw" -> go (drop 2 path) ("nw" : acc)
        "ne" -> go (drop 2 path) ("ne" : acc)
        (first : _) | first == 'e' ->  go (drop 1 path) ("e" : acc)
        (first : _) | first == 'w' ->  go (drop 1 path) ("w" : acc)

traversePath :: Coordinate -> Path -> [Coordinate]
traversePath originalPosition path =
   go originalPosition path [originalPosition] where
     go :: Coordinate -> Path -> [Coordinate] -> [Coordinate]
     go current [] acc = acc
     go coordinate@(column, row) (step : rest) acc = do
       let neighbours = adjacent coordinate
       let updateCoordinate = neighbours Map.! step
       go updateCoordinate rest (updateCoordinate : acc)

adjacent :: Coordinate -> Map.Map String Coordinate
adjacent (column, row) = Map.fromList [
    ("e", (column + 1, row)),
    ("w", (column - 1, row)),
    ("ne", if even row then (column, row - 1) else (column + 1, row - 1)),
    ("nw", if even row then (column - 1, row - 1) else (column, row - 1)),
    ("se", if even row then (column, row + 1) else (column + 1, row + 1)),
    ("sw", if even row then (column - 1, row + 1) else (column, row + 1))
  ]