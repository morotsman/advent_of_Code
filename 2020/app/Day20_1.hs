{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (nub)
import MatrixUtil (Matrix(..), rotateMatrix, flipMatrixHorizontally, flipMatrixVertically, getRow, getColumn, printMatrix)

type Answer = [String]

data Tile = Tile {
  tileId :: Int,
  tileMatrix :: Matrix Char
} deriving (Show, Eq)

data Match = TopMatch {
    tile1 :: Tile,
    tile2 :: Tile
  }
  | BottomMatch {
    matrix1 :: Tile,
    matrix2 :: Tile
  }
  | LeftMatch {
    matrix1 :: Tile,
    matrix2 :: Tile
  }
  | RightMatch {
    matrix1 :: Tile,
    matrix2 :: Tile
  } deriving (Show, Eq)

matrix2311 :: Matrix Char
matrix2311 = Matrix
          ["..##.#..#."
         , "##..#....."
         , "#...##..#."
         , "####.#...#"
         , "##.##.###."
         , "##...#.###"
         , ".#.#.#..##"
         , "..#....#.."
         , "###...#.#."
         , "..###..###"
         ]

tile2311 :: Tile
tile2311 = Tile 2311 matrix2311

matrix1951 :: Matrix Char
matrix1951 = Matrix
  [        "#.##...##."
         , "#.####...#"
         , ".....#..##"
         , "#...######"
         , ".##.#....#"
         , ".###.#####"
         , "###.##.##."
         , ".###....#."
         , "..#.#..#.#"
         , "#...##.#.."
         ]

tile1951 :: Tile
tile1951 = Tile 1951 matrix1951

tile2729 :: Tile
tile2729 = Tile {
  tileId = 2729,
  tileMatrix = Matrix
    [ "...#.#.#.#"
    , "####.#...."
    , "..#.#....."
    , "....#..#.#"
    , ".##..##.#."
    , ".#.####..."
    , "####.#.#.."
    , "##.####..."
    , "##..#.##.."
    , "#.##...##."
    ]
}

tile1427 :: Tile
tile1427 = Tile {
  tileId = 1427,
  tileMatrix = Matrix
    [ "###.##.#.."
    , ".#..#.##.."
    , ".#.##.#..#"
    , "#.#.#.##.#"
    , "....#...##"
    , "...##..##."
    , "...#.#####"
    , ".#.####.#."
    , "..#..###.#"
    , "..##.#..#."
    ]
}

solveIt :: [String] -> Answer
solveIt input = input

type PuzzleSolution = Matrix (Maybe Tile)

solvePuzzle :: [Tile] -> Int -> [PuzzleSolution]
solvePuzzle [] _ = []
solvePuzzle (t : []) _ = [Matrix [[Just(t)]]]
solvePuzzle (t : ts) length = do
  puzzleSolution <- solvePuzzle ts length
  let extendedPuzzleSolution = addTileToPuzzle t puzzleSolution
  filter validPuzzleSolution extendedPuzzleSolution

addTileToPuzzle :: Tile -> PuzzleSolution -> [PuzzleSolution]
addTileToPuzzle tile puzzleSolution = [puzzleSolution]

validPuzzleSolution :: PuzzleSolution -> Bool
validPuzzleSolution puzzleSolution = True

findAllMatchingEdgePositions :: Tile -> Tile -> [Match]
findAllMatchingEdgePositions matrix1 matrix2 = do
    matrix1 <- allTransformations matrix1
    matrix2 <- allTransformations matrix2
    findMatchingEdgePositions matrix1 matrix2

allTransformations :: Tile -> [Tile]
allTransformations t@(Tile _ matrix) = let
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

findMatchingEdgePositions :: Tile -> Tile -> [Match]
findMatchingEdgePositions t1@(Tile _ m1) t2@(Tile _ m2) = let
  matchLeft = if (leftColumn m1 == rightColumn m2) then [LeftMatch t1 t2] else []
  matchRight = if (rightColumn m1 == leftColumn m2) then [RightMatch t1 t2] else []
  matchTop = if (topRow m1 == bottomRow m2) then [TopMatch t1 t2] else []
  matchBottom = if (bottomRow m1 == topRow m2) then [BottomMatch t1 t2] else []
  in
    matchLeft ++ matchRight ++ matchTop ++ matchBottom

matchWithoutMatrix :: Match -> Match
matchWithoutMatrix (TopMatch t1 t2) = TopMatch (removeMatrix t1) (removeMatrix t2)
matchWithoutMatrix (BottomMatch t1 t2) = BottomMatch (removeMatrix t1) (removeMatrix t2)
matchWithoutMatrix (LeftMatch t1 t2) = LeftMatch (removeMatrix t1) (removeMatrix t2)
matchWithoutMatrix (RightMatch t1 t2) = RightMatch (removeMatrix t1) (removeMatrix t2)

removeMatrix :: Tile -> Tile
removeMatrix (Tile id _) = Tile id (Matrix [])

topRow :: Matrix a -> [a]
topRow matrix = getRow 9 matrix

bottomRow :: Matrix a -> [a]
bottomRow matrix = getRow 0 matrix

leftColumn :: Matrix a -> [a]
leftColumn matrix = getColumn 0 matrix

rightColumn :: Matrix a -> [a]
rightColumn matrix = getColumn 9 matrix

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/app/Day20_example.txt"
  let linesOfFile = lines content
  let answer = solveIt linesOfFile
--  print (length (findAllMatchingEdgePositions matrix1951 matrix1951))
  --print (fmap matchWithoutMatrix (findAllMatchingEdgePositions tile1951 tile2311))
  --print (length (findAllMatchingEdgePositions tile1951 tile2311))
  print (solvePuzzle [tile1951, tile1427, tile2729, tile2311] 2)

