{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (nub)
import Data.Maybe (fromJust, isJust)
import MatrixUtil (Matrix(..), MatrixDimension(..), rotateMatrix, flipMatrixHorizontally, flipMatrixVertically, getRow, getColumn, printMatrix, matrixDimension, elementAt)
import Debug.Trace (trace)
import qualified Data.Map as Map

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
  in
    concatMap (\ps -> solvePuzzle ps (tilesNotInSolution ps tiles) matrixLength) possibleExtensions2

validPuzzleSolution :: Int -> PuzzleSolution -> Bool
validPuzzleSolution matrixLength puzzleSolution = let
  (solutionLength, solutionHeight) = getPuzzleDimension puzzleSolution
  in solutionLength <= matrixLength && solutionHeight <= matrixLength

getPuzzleDimension :: PuzzleSolution -> (Int, Int)
getPuzzleDimension puzzleSolution = let
  tiles = Map.elems puzzleSolution
  positions = fmap (\tile -> fromJust (position tile)) tiles
  xPositions = fmap (\(x, _) -> x) positions
  yPositions = fmap (\(_, y) -> y) positions
  minX = minimum xPositions
  maxX = maximum xPositions
  minY = minimum yPositions
  maxY = maximum yPositions
  in (maxX - minX, maxY - minY)


tilesNotInSolution :: PuzzleSolution -> [Tile] -> [Tile]
tilesNotInSolution  puzzleSolution tiles = filter (\t -> not $ tileAlreadyInSolution puzzleSolution t) tiles

addTileToPuzzle :: Tile -> PuzzleSolution -> [PuzzleSolution]
addTileToPuzzle tileToAdd puzzleSolution | Map.null puzzleSolution = [getInitialSolution tileToAdd]
addTileToPuzzle tileToAdd puzzleSolution = let
    tilesInSolution = Map.elems puzzleSolution
    matchingTiles :: [(Tile, Tile)] = concatMap (\tile -> findAllMatchingEdgePositions tile tileToAdd) tilesInSolution
    result = fmap (\(t1, t2) -> insertTile puzzleSolution t2) matchingTiles
    in
    --trace(
    --"\naddTileToPuzzle: \ntileToAdd: " ++ show tileToAdd ++
    --"\npuzzleSolution: " ++ show puzzleSolution ++
    --"\nmatchingTiles: " ++ (show (fmap (\(t1, t2) -> "(" ++ "one: " ++ show t1 ++ ", two: " ++ show t2 ++ ")") matchingTiles)) ++
    --"\nresult: " ++ show result ++ "\n"
    --)
    result

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
  result = matchLeft ++ matchRight ++ matchTop ++ matchBottom
  in
    --trace("\nfindMatchingEdgePositions: " ++ (show (fmap (\(t1, t2) -> "(" ++ show t1 ++ ", " ++ show t2 ++ ")") result)))
    result

removeMatrix :: Tile -> Tile
removeMatrix t@(Tile id _ position) = t { tileMatrix = Matrix [] }

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

printSolution :: PuzzleSolution -> [(Int, Position)]
printSolution puzzleSolution = let
  tiles = Map.elems puzzleSolution
  positions = fmap (\tile -> (tileId tile, fromJust (position tile))) tiles
  in positions

tileWithoutMatrix :: Tile -> Tile
tileWithoutMatrix t@(Tile id _ position) = t { tileMatrix = Matrix [] }

toTile :: [String] -> Tile
toTile (idLine : matrixLines) = let
  id = read $ take 4 $ drop 5 idLine
  matrix = Matrix matrixLines
  in Tile id matrix Nothing

parseTiles :: Int -> [String] -> [Tile]
parseTiles size [] = []
parseTiles size (idLine : lines) = let
  id = read (parseId idLine)
  matrixLines = take size lines
  matrix = Matrix matrixLines
  in (Tile id matrix Nothing) : parseTiles size (drop (size+1) lines)

parseId :: String -> String
parseId idLine = let
  result = take 4 $ drop 5 idLine
  in trace("id: " ++ result) result

smallExample :: [String] = [
  "Tile 1111:",
  "aaa",
  "aaa",
  "bab",
  "",
  "Tile 2222:",
  "bab",
  "bbb",
  "c1c",
  "",
  "Tile 3333:",
  "c1c",
  "ccc",
  "dcd",
  ""
  ]

smallExample2 :: [String] = [
  "Tile 1111:",
  "aaa",
  "aaa",
  "bab",
  "",
  "Tile 2222:",
  "bab",
  "bbb",
  "c1c",
  ""
  ]

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/app/Day20_example.txt"
  --content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/app/Day20_input.txt"
  let linesOfFile = lines content

  let tiles = parseTiles 10 linesOfFile
  --print ("tiles: " ++ show (length tiles))


  print ("tiles: " ++ show tiles)
  let solution :: PuzzleSolution = head (solvePuzzle Map.empty tiles 3)
  print ("final result: " ++ show (printSolution solution))



  --let tiles = parseTiles 3 smallExample
  --let solutions1 :: [PuzzleSolution] = addTileToPuzzle (tiles !! 0) Map.empty
  --print ("solution1: " ++ show solutions1)
  --let solutions2 :: [PuzzleSolution]  = concatMap (\s -> addTileToPuzzle (tiles !! 1) s) solutions1
  --print ("solution2: " ++ show solutions2)
  --let solutions3 = concatMap (\s -> addTileToPuzzle (tiles !! 2) s) solutions2
  --print ("solution3: " ++ show (solutions3))

  --let tiles = parseTiles 3 smallExample
  --let solutions = solvePuzzle Map.empty tiles 2
  --print ("\n final result: " ++ show solutions)
  --print("hepp")





