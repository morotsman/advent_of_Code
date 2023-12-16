{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (nub)
import MatrixUtil (Matrix(..), rotateMatrix, flipMatrixHorizontally, flipMatrixVertically, getRow, getColumn, printMatrix)

type Answer = [String]

data Tile = Tile {
  tileId :: Int,
  tileMatrix :: Matrix Char
}

data Match = Top | Bottom | Left | Right

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

solveIt :: [String] -> Answer
solveIt input = input

topRow :: Matrix a -> [a]
topRow matrix = getRow 9 matrix

bottomRow :: Matrix a -> [a]
bottomRow matrix = getRow 0 matrix

leftColumn :: Matrix a -> [a]
leftColumn matrix = getColumn 0 matrix

rightColumn :: Matrix a -> [a]
rightColumn matrix = getColumn 9 matrix

findAllMatchingEdgePositions :: Matrix Char -> Matrix Char -> [(Matrix Char, Matrix Char)]
findAllMatchingEdgePositions matrix1 matrix2 = do
    matrix1 <- allTransformations matrix1
    matrix2 <- allTransformations matrix2
    findMatchingEdgePositions matrix1 matrix2

allTransformations :: Matrix Char -> [Matrix Char]
allTransformations matrix =
  nub $
    [ matrix
    , rotateMatrix matrix
    , rotateMatrix (rotateMatrix matrix)
    , rotateMatrix (rotateMatrix (rotateMatrix matrix))
    , flipMatrixHorizontally matrix
    , flipMatrixVertically matrix
    , rotateMatrix (flipMatrixHorizontally matrix)
    , rotateMatrix (flipMatrixVertically matrix)
    ]
    
findMatchingEdgePositions :: Matrix Char -> Matrix Char -> [(Matrix Char, Matrix Char)]
findMatchingEdgePositions m1 m2 = let
  matchLeft = if (leftColumn m1 == rightColumn m2) then [(m1, m2)] else []
  matchRight = if (rightColumn m1 == leftColumn m2) then [(m1, m2)] else []
  matchTop = if (topRow m1 == bottomRow m2) then [(m1, m2)] else []
  matchBottom = if (bottomRow m1 == topRow m2) then [(m1, m2)] else []
  in
    matchLeft ++ matchRight ++ matchTop ++ matchBottom

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/app/Day20_example.txt"
  let linesOfFile = lines content
  let answer = solveIt linesOfFile
  print (length (findAllMatchingEdgePositions matrix1951 matrix1951))
  print (length (findAllMatchingEdgePositions matrix1951 matrix2311))
  print matrix2311
  printMatrix matrix2311


