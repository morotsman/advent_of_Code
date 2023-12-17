{-# LANGUAGE ScopedTypeVariables #-}
module MatrixUtil (Matrix(..), MatrixDimension(..), rotateMatrix, flipMatrixHorizontally, flipMatrixVertically, getRow, getColumn, printMatrix, matrixDimension, elementAt) where

import Data.List (transpose)

data Matrix a = Matrix [[a]] deriving (Show, Eq)

data MatrixDimension = MatrixDimension {
  matrixLength :: Int,
  matrixHeight :: Int
}

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

elementAt :: Int -> Int -> Matrix a -> a
elementAt x y (Matrix matrix) = (matrix !! y) !! x
