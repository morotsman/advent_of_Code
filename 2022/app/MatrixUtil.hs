{-# LANGUAGE ScopedTypeVariables #-}
module MatrixUtil (Matrix(..), MatrixDimension(..), rotateMatrix, flipMatrixHorizontally, flipMatrixVertically, getRow, getColumn, printMatrix, matrixDimension, elementAt, matrixValues, findElement, Coordinate, outOfBoundary, findElements) where

import Data.List (transpose)

data Matrix a = Matrix [[a]] deriving (Show, Eq)

data MatrixDimension = MatrixDimension {
  matrixLength :: Int,
  matrixHeight :: Int
}

-- Column, Row
type Coordinate = (Int, Int)

matrixValues :: Matrix a -> [[a]]
matrixValues (Matrix matrix) = matrix

rotateMatrix :: Matrix a -> Matrix a
rotateMatrix (Matrix matrix) = Matrix (transpose . reverse $ matrix)

flipMatrixHorizontally :: Matrix a -> Matrix a
flipMatrixHorizontally (Matrix matrix) = Matrix (map reverse matrix)

flipMatrixVertically :: Matrix a -> Matrix a
flipMatrixVertically (Matrix matrix) = Matrix (reverse matrix)

getRow :: Int -> Matrix a -> [a]
getRow index (Matrix matrix)
  | index < 0 || index >= length matrix = error "Invalid row index"
  | otherwise = matrix !! index

getColumn :: Int -> Matrix a -> [a]
getColumn index (Matrix matrix)
  | index < 0 || null matrix || any (\row -> index >= length row) matrix = error "Invalid column index"
  | otherwise = reverse (map (!! index) matrix)

printMatrix :: Matrix Char -> IO ()
printMatrix (Matrix matrix) = mapM_ putStrLn matrix

matrixDimension :: Matrix a -> MatrixDimension
matrixDimension (Matrix matrix) = MatrixDimension (length $ head matrix) (length matrix)

outOfBoundary :: Int -> Int -> Matrix a -> Bool
outOfBoundary x y matrix = let
  (MatrixDimension length width) = matrixDimension matrix
  in x < 0 || x >= length || y < 0 || y >= width

elementAt :: Matrix a -> Int -> Int -> Maybe a
elementAt m@(Matrix matrix) x y | outOfBoundary x y m = Nothing
                                | otherwise = Just ((matrix !! y) !! x)

findElement :: Matrix a -> (a -> Bool) -> Maybe Coordinate
findElement matrix predicate = go 0 0
  where
    MatrixDimension width height = matrixDimension matrix

    go :: Int -> Int -> Maybe Coordinate
    go x y
      | y >= height = Nothing  -- We've gone past the last row
      | x >= width  = go 0 (y + 1)  -- Move to the next row
      | otherwise =
          case elementAt matrix x y of
            Just element | predicate element -> Just (x, y)  -- Found a match
                         | otherwise -> go (x + 1) y
            Nothing -> go (x + 1) y  -- Continue searching

findElements :: Matrix a -> (a -> Bool) -> [Coordinate]
findElements matrix predicate = go 0 0
  where
    MatrixDimension width height = matrixDimension matrix

    go :: Int -> Int -> [Coordinate]
    go x y
      | y >= height = []  -- We've gone past the last row
      | x >= width  = go 0 (y + 1)  -- Move to the next row
      | otherwise =
          case elementAt matrix x y of
            Just element | predicate element -> (x, y) : go (x + 1) y  -- Found a match
                         | otherwise -> go (x + 1) y
            Nothing -> go (x + 1) y  -- Continue searching