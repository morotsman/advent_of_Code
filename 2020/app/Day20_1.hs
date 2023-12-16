{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (transpose, nub)
import MatrixUtil (Matrix(..), rotateMatrix, flipMatrixHorizontally, flipMatrixVertically, getRow, getColumn, printMatrix)

type Answer = [String]

data Tile = Tile {
  tileId :: Int,
  tileMatrix :: Matrix Char
}

data Match = Top | Bottom | Left | Right

matrix2311 :: Matrix Char
matrix2311 =
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
matrix1951 =
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

topRow :: [[a]] -> [a]
topRow matrix = getRow 9 matrix

bottomRow :: [[a]] -> [a]
bottomRow matrix = getRow 0 matrix

leftColumn :: [[a]] -> [a]
leftColumn matrix = getColumn 0 matrix

rightColumn :: [[a]] -> [a]
rightColumn matrix = getColumn 9 matrix

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

findAllMatchingEdgePositions :: Matrix Char -> Matrix Char -> [(Matrix Char, Matrix Char)]
findAllMatchingEdgePositions matrix1 matrix2 = do
    matrix1 <- allTransformations matrix1
    matrix2 <- allTransformations matrix2
    findMatchingEdgePositions matrix1 matrix2


findMatchingEdgePositions :: Matrix Char -> Matrix Char -> [(Matrix Char, Matrix Char)]
findMatchingEdgePositions matrix1 matrix2 = let
  matchLeft = if (leftColumn matrix1 == rightColumn matrix2) then [(matrix1, matrix2)] else []
  matchRight = if (rightColumn matrix1 == leftColumn matrix2) then [(matrix1, matrix2)] else []
  matchTop = if (topRow matrix1 == bottomRow matrix2) then [(matrix1, matrix2)] else []
  matchBottom = if (bottomRow matrix1 == topRow matrix2) then [(matrix1, matrix2)] else []
  in
    matchLeft ++ matchRight ++ matchTop ++ matchBottom

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2020/app/Day20_example.txt"
  let linesOfFile = lines content
  let answer = solveIt linesOfFile
  print (length (findAllMatchingEdgePositions matrix1951 matrix1951))
  printMatrix matrix2311


