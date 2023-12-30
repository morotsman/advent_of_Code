{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import MatrixUtil (Matrix(..), MatrixDimension(..), rotateMatrix, flipMatrixHorizontally, flipMatrixVertically, getRow, getColumn, printMatrix, matrixDimension, elementAt, matrixValues)
import Debug.Trace (trace)
import qualified Data.Map as Map
import Text.Regex.TDFA ((=~), AllTextMatches, getAllTextMatches)
import qualified Data.Set as Set

type Answer = [String]

type Position = (Int, Int)

data Tile = Tile {
  tileId :: Int,
  tileMatrix :: Matrix Char,
  position :: Maybe Position
} deriving (Show, Eq)

type PuzzleSolution = Map.Map Position Tile

solvePuzzle :: PuzzleSolution -> [Tile] -> Int -> [PuzzleSolution]
solvePuzzle puzzleSolution (tile : []) _ = addTileToPuzzle tile puzzleSolution
solvePuzzle puzzleSolution tiles matrixLength = let
  extendedPuzzleSolutions :: [PuzzleSolution] = concatMap (\t -> addTileToPuzzle t puzzleSolution) tiles
  puzzlesWihPossibleExtensionTiles :: [(PuzzleSolution, [Tile])] = fmap (\ps -> (ps, tilesNotInSolution ps tiles)) extendedPuzzleSolutions
  possibleExtensions :: [PuzzleSolution] = map (\(p, t) -> p)  (filter (\(_, t) -> (length tiles) > (length t)) puzzlesWihPossibleExtensionTiles)
  possibleExtensions2 :: [PuzzleSolution] = (filter (validPuzzleSolution matrixLength) possibleExtensions)
  in concatMap (\ps -> solvePuzzle ps (tilesNotInSolution ps tiles) matrixLength) possibleExtensions2

validPuzzleSolution :: Int -> PuzzleSolution -> Bool
validPuzzleSolution matrixLength puzzleSolution = let
  (solutionLength, solutionHeight) = getPuzzleDimension puzzleSolution
  in solutionLength <= matrixLength && solutionHeight <= matrixLength

getPuzzleDimension :: PuzzleSolution -> (Int, Int)
getPuzzleDimension puzzleSolution = let
  (minX, maxX, minY, maxY) = getMaxAndMinPositions puzzleSolution
  in (maxX - minX, maxY - minY)

getMaxAndMinPositions :: PuzzleSolution -> (Int, Int, Int, Int)
getMaxAndMinPositions puzzleSolution = let
  tiles = Map.elems puzzleSolution
  positions = fmap (\tile -> fromJust (position tile)) tiles
  xPositions = fmap (\(x, _) -> x) positions
  yPositions = fmap (\(_, y) -> y) positions
  in (minimum xPositions, maximum xPositions, minimum yPositions, maximum yPositions)

tilesNotInSolution :: PuzzleSolution -> [Tile] -> [Tile]
tilesNotInSolution  puzzleSolution tiles = filter (\t -> not $ tileAlreadyInSolution puzzleSolution t) tiles

addTileToPuzzle :: Tile -> PuzzleSolution -> [PuzzleSolution]
addTileToPuzzle tileToAdd puzzleSolution | Map.null puzzleSolution = [getInitialSolution tileToAdd]
addTileToPuzzle tileToAdd puzzleSolution = let
    tilesInSolution = Map.elems puzzleSolution
    matchingTiles :: [(Tile, Tile)] = concatMap (\tile -> findAllMatchingEdgePositions tile tileToAdd) tilesInSolution
    in fmap (\(t1, t2) -> insertTile puzzleSolution t2) matchingTiles

getInitialSolution :: Tile -> PuzzleSolution
getInitialSolution tile = Map.singleton (0, 0) (tile { position = Just (0, 0) })

insertTile :: PuzzleSolution -> Tile -> PuzzleSolution
insertTile puzzleSolution t@(Tile _ _ Nothing) = puzzleSolution
insertTile puzzleSolution t@(Tile _ _ (Just(position))) | isJust $ Map.lookup position puzzleSolution = puzzleSolution
                                                        | otherwise = Map.insert position t puzzleSolution

tileAlreadyInSolution :: PuzzleSolution -> Tile -> Bool
tileAlreadyInSolution puzzleSolution tile = let
  tiles = Map.elems puzzleSolution
  in any (\t -> tileId t == tileId tile) tiles

findAllMatchingEdgePositions :: Tile -> Tile -> [(Tile, Tile)]
findAllMatchingEdgePositions tile tileToAdd = do
    transformed2 <- allTransformations tileToAdd
    findMatchingEdgePositions tile transformed2

allTransformations :: Tile -> [Tile]
allTransformations t@(Tile _ matrix _) = let
  transformations = nub $
    [ matrix
    , rotateMatrix matrix
    , rotateMatrix (rotateMatrix matrix)
    , rotateMatrix (rotateMatrix (rotateMatrix matrix))
    , flipMatrixHorizontally matrix
    , flipMatrixVertically matrix
    , rotateMatrix (flipMatrixHorizontally matrix)
    , rotateMatrix (flipMatrixVertically matrix)
    ]
  in fmap (\m -> t { tileMatrix = m }) transformations

findMatchingEdgePositions :: Tile -> Tile -> [(Tile, Tile)]
findMatchingEdgePositions t1@(Tile _ m1 Nothing) _ = []
findMatchingEdgePositions t1@(Tile _ m1 (Just(x, y))) t2@(Tile _ m2 _) = let
  matchLeft = if (leftColumn m1 == rightColumn m2) then [(t1, (t2 { position = Just (x - 1, y) }))] else []
  matchRight = if (rightColumn m1 == leftColumn m2) then [(t1, (t2 { position = Just (x + 1, y) }))] else []
  matchTop = if (topRow m1 == bottomRow m2) then [(t1, (t2 { position = Just (x, y + 1) }))] else []
  matchBottom = if (bottomRow m1 == topRow m2) then [(t1, (t2 { position = Just (x, y - 1) }))] else []
  in matchLeft ++ matchRight ++ matchTop ++ matchBottom

topRow :: Matrix a -> [a]
topRow matrix = let
  (MatrixDimension length _) = matrixDimension matrix
  in getRow (length-1) matrix

bottomRow :: Matrix a -> [a]
bottomRow matrix = getRow 0 matrix

leftColumn :: Matrix a -> [a]
leftColumn matrix = getColumn 0 matrix

rightColumn :: Matrix a -> [a]
rightColumn matrix = let
  (MatrixDimension length _) = matrixDimension matrix
  in getColumn (length-1) matrix

parseTiles :: Int -> [String] -> [Tile]
parseTiles size [] = []
parseTiles size (idLine : lines) = let
  id = read (parseId idLine)
  matrixLines = take size lines
  matrix = Matrix matrixLines
  in (Tile id matrix Nothing) : parseTiles size (drop (size+1) lines)

parseId :: String -> String
parseId idLine = take 4 $ drop 5 idLine

puzzleSolutionToMatrix :: PuzzleSolution -> Matrix Char
puzzleSolutionToMatrix puzzleSolution = let
  (minX, maxX, minY, maxY) = getMaxAndMinPositions puzzleSolution
  coordinates = [(x, y) | x <- [minX..maxX], y <- [minY..maxY]]
  sideLength = squareRoot $ length coordinates
  columns = groupBy sideLength coordinates
  tileColumns = fmap (fmap (\position -> removeBorders (fromJust $ Map.lookup position puzzleSolution))) columns
  merged = mergeColumns (fmap mergeRows tileColumns) []
  in flipMatrixVertically (Matrix merged)

removeBorders :: Tile -> Tile
removeBorders t@(Tile id matrix position) = let
  (MatrixDimension length height) = matrixDimension matrix
  in t { tileMatrix = Matrix (removeFirstAndLastRow (removeFirstAndLastColumn (matrixValues matrix))) }

removeFirstAndLastRow :: [[a]] -> [[a]]
removeFirstAndLastRow matrix = let
  (MatrixDimension length height) = matrixDimension (Matrix matrix)
  in drop 1 $ take (height-1) matrix

removeFirstAndLastColumn :: [[a]] -> [[a]]
removeFirstAndLastColumn matrix = let
  (MatrixDimension length height) = matrixDimension (Matrix matrix)
  in fmap (drop 1 . take (length-1)) matrix

mergeRows :: [Tile] -> [[Char]]
mergeRows [] = []
mergeRows (t:ts) = let
  matrix :: Matrix Char = tileMatrix t
  chars = matrixValues (tileMatrix t)
  in chars ++ (mergeRows ts)

mergeColumns :: [[[Char]]] -> [[Char]] -> [[Char]]
mergeColumns [] result = result
mergeColumns (c:cs) [] = mergeColumns cs c
mergeColumns (c:cs) result = let
  merged :: [[Char]] = zipWith (++) result c
  in mergeColumns cs merged

squareRoot :: Int -> Int
squareRoot n = floor $ sqrt $ fromIntegral n

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n xs = let
  (first, rest) = splitAt n xs
  in first : groupBy n rest

calculateRoughness :: PuzzleSolution -> Int
calculateRoughness puzzleSolution = let
  image = puzzleSolutionToMatrix puzzleSolution
  monsterSize = 15
  numberOfBrackets = countBracketsInRows (matrixValues image)
  numberOfSeaMonsters = countSeaMonsters puzzleSolution
  in numberOfBrackets - numberOfSeaMonsters * monsterSize

countSeaMonsters :: PuzzleSolution -> Int
countSeaMonsters puzzleSolution = let
  image = puzzleSolutionToMatrix puzzleSolution
  images = fmap (\t -> tileMatrix t) (allTransformations (Tile 0 image Nothing))
  results = fmap (\image -> countSeaMonsters' image [] 0) images
  in maximum results

countSeaMonsters' :: Matrix Char -> [[Char]] -> Int -> Int
countSeaMonsters' (Matrix []) rows total | length rows < 3 = 0
countSeaMonsters' (Matrix []) rows total = total + findSeaMonster rows
countSeaMonsters' (Matrix (r:rs)) rows total | length rows < 3 = countSeaMonsters' (Matrix rs) (rows ++ [r]) total
countSeaMonsters' (Matrix (r:rs)) rows total = let
  seaMonsterCount = findSeaMonster rows
  toInvestigate = (drop 1 rows) ++ [r]
  in countSeaMonsters' (Matrix rs) toInvestigate (total + seaMonsterCount)

findSeaMonster :: [[Char]] -> Int
findSeaMonster rows = let
  middlePattern = "(#....##....##....###)"
  bottomPattern = "(#..#..#..#..#..#...)"
  topPattern = "(#)"
  matchMiddleIndexes = findAllMatchIndexes middlePattern (rows !! 1) 0
  bottomIndexes = Set.fromList $ findAllMatchIndexes bottomPattern (rows !! 2) 0
  topIndexes = Set.fromList $ filter (\index -> if (index >= length (rows !! 0)) then False else ((rows !! 0) !! index) == '#') $ fmap (\middleIndex -> middleIndex + 18) matchMiddleIndexes
  monsters = filter (\index -> Set.member (index + 1) bottomIndexes && Set.member (index + 18) topIndexes) matchMiddleIndexes
  in length monsters

countBrackets :: String -> Int
countBrackets [] = 0
countBrackets (c:cs) | c == '#' = 1 + countBrackets cs
                      | otherwise = countBrackets cs

countBracketsInRows :: [[Char]] -> Int
countBracketsInRows [] = 0
countBracketsInRows (r:rs) = countBrackets r + countBracketsInRows rs

findAllMatchIndexes :: String -> String -> Int -> [Int]
findAllMatchIndexes _ "" _ = []
findAllMatchIndexes pattern input@(i:is) startIndex =
    case input =~ pattern :: (String, String, String, [String]) of
        (pre, match, post, _) ->
            if not (startsWithPattern input pattern) then findAllMatchIndexes pattern is (startIndex + 1)
            else startIndex : findAllMatchIndexes pattern is (startIndex + 1)

startsWithPattern :: String -> String -> Bool
startsWithPattern input pattern = input =~ ("^" ++ pattern) :: Bool

main :: IO ()
main = do
  --content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/app/Day20_example.txt"
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/app/Day20_input.txt"
  let linesOfFile = lines content
  let tiles = parseTiles 10 linesOfFile
  --let solution :: PuzzleSolution = head (solvePuzzle Map.empty tiles 3)
  let solution :: PuzzleSolution = head (solvePuzzle Map.empty tiles 12)
  print("Solved puzzle")

  print ("roughness: " ++ show (calculateRoughness solution))






