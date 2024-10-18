{-# LANGUAGE ScopedTypeVariables #-}
module Challenge1( Coordinate, Path, getPath, traversePath, visitedTiles, blackTiles, whiteTiles, flipIt) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (Set)

-- column, row
type Column = Int
type Row = Int
-- offset coordinates as suggested here: https://math.stackexchange.com/questions/2254655/hexagon-grid-coordinate-system
type Coordinate = (Column, Row)

type Path = [String]

type Tiles = Set Coordinate
type BlackTiles = Tiles
type WhiteTiles = Tiles
type Floor = (BlackTiles, WhiteTiles)

-- Any black tile with zero or more than 2 black tiles immediately adjacent to it is flipped to white.
-- Any white tile with exactly 2 black tiles immediately adjacent to it is flipped to black.

flipIt :: Floor -> Int -> Floor
flipIt floor 0 = floor
flipIt floor n = flipIt (flipTiles floor) (n-1)

flipTiles :: Floor -> Floor
flipTiles (blackTiles, whiteTiles) = let
    (stillBlackTile, flipToWhite) = Set.partition (keepAsBlack blackTiles) blackTiles
    (stillWhiteTile, flipToBlack) = Set.partition (keepAsWight blackTiles) whiteTiles
    updatedBlackTiles = Set.union stillBlackTile flipToBlack
    updatedWhiteTiles = Set.union (neighbouringWhiteTiles updatedBlackTiles) (Set.union flipToWhite stillWhiteTile)
  in (updatedBlackTiles, updatedWhiteTiles)

keepAsBlack :: Tiles -> Coordinate -> Bool
keepAsBlack tiles tile = let
  numberOfBlackAdjacentTiles = colorsOfAdjacent tiles tile
  in numberOfBlackAdjacentTiles == 1 || numberOfBlackAdjacentTiles == 2

keepAsWight :: Tiles -> Coordinate -> Bool
keepAsWight tiles tile = let
  numberOfBlackAdjacentTiles = colorsOfAdjacent tiles tile
  in not $ numberOfBlackAdjacentTiles == 2

colorsOfAdjacent :: Tiles -> Coordinate -> Int
colorsOfAdjacent tiles tile = let
  adjacentTiles = Map.elems $ adjacent tile
  in foldr (\tile acc -> if Set.member tile tiles then acc + 1 else acc) 0 adjacentTiles

blackTiles :: Map.Map Coordinate Int -> Set Coordinate
blackTiles visitedTiles = Set.fromList $ Map.keys $ Map.filter odd visitedTiles

whiteTiles :: Map.Map Coordinate Int -> Set Coordinate
whiteTiles visitedTiles = let
  blackTiles' = blackTiles visitedTiles
  knownWhiteTiles = Set.fromList $ Map.keys $ Map.filter even visitedTiles
  in Set.union knownWhiteTiles $ neighbouringWhiteTiles blackTiles'

neighbouringWhiteTiles :: Set Coordinate -> Set Coordinate
neighbouringWhiteTiles blackTiles' =
  Set.fromList $ filter (flip Set.notMember blackTiles') $ concatMap (Map.elems . adjacent) $ Set.toList blackTiles'

visitedTiles :: [String] -> Map.Map Coordinate Int
visitedTiles lines = do
  countOccurrences $ map (\line -> head $ traversePath (0, 0) $ getPath line) lines


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