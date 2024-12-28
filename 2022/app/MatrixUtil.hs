{-# LANGUAGE ScopedTypeVariables #-}
module MatrixUtil (Matrix(..), MatrixDimension(..), rotateMatrix, flipMatrixHorizontally, flipMatrixVertically, getRow, getColumn, printMatrix, matrixDimension, elementAt, matrixValues, findElement, Coordinate ( .. ), outOfBoundary, findElements) where

import Data.List (transpose)

data Matrix a = Matrix [[a]] deriving (Show, Eq)

data MatrixDimension = MatrixDimension {
  matrixWidth :: Int,
  matrixHeight :: Int
}

-- Column, Row
data Coordinate = Coordinate {
  column :: Int,
  row :: Int
} deriving (Show, Eq, Ord)

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
outOfBoundary column row matrix = let
  (MatrixDimension width height) = matrixDimension matrix
  in column < 0 || column >= width || row < 0 || row >= height

elementAt :: Matrix a -> Int -> Int -> Maybe a
elementAt m@(Matrix matrix) column row | outOfBoundary column row m = Nothing
                                | otherwise = Just ((matrix !! row) !! column)

findElement :: Matrix a -> (a -> Bool) -> Maybe Coordinate
findElement matrix predicate = go 0 0
  where
    MatrixDimension width height = matrixDimension matrix

    go :: Int -> Int -> Maybe Coordinate
    go column row
      | row >= height = Nothing  -- We've gone past the last row
      | column >= width  = go 0 (row + 1)  -- Move to the next row
      | otherwise =
          case elementAt matrix column row of
            Just element | predicate element -> Just (Coordinate column row)  -- Found a match
                         | otherwise -> go (column + 1) row
            Nothing -> go (column + 1) row  -- Continue searching

findElements :: Matrix a -> (a -> Bool) -> [Coordinate]
findElements matrix predicate = go 0 0
  where
    MatrixDimension width height = matrixDimension matrix

    go :: Int -> Int -> [Coordinate]
    go column row
      | row >= height = []  -- We've gone past the last row
      | column >= width  = go 0 (row + 1)  -- Move to the next row
      | otherwise =
          case elementAt matrix column row of
            Just element | predicate element -> (Coordinate column row) : go (column + 1) row  -- Found a match
                         | otherwise -> go (column + 1) row
            Nothing -> go (column + 1) row  -- Continue searching