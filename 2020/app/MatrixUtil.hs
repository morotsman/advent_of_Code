{-# LANGUAGE ScopedTypeVariables #-}
module MatrixUtil (Matrix(..), rotateMatrix, flipMatrixHorizontally, flipMatrixVertically, getRow, getColumn, printMatrix) where

import Data.List (transpose, nub)

type Matrix a = [[a]]

rotateMatrix :: [[a]] -> [[a]]
rotateMatrix = transpose . reverse

flipMatrixHorizontally :: [[a]] -> [[a]]
flipMatrixHorizontally = map reverse

flipMatrixVertically :: [[a]] -> [[a]]
flipMatrixVertically = reverse

getRow :: Int -> [[a]] -> [a]
getRow index matrix
  | index < 0 || index >= length matrix = error "Invalid row index"
  | otherwise = matrix !! index

getColumn :: Int -> [[a]] -> [a]
getColumn index matrix
  | index < 0 || null matrix || any (\row -> index >= length row) matrix = error "Invalid column index"
  | otherwise = reverse (map (!! index) matrix)

printMatrix :: Matrix Char -> IO ()
printMatrix = mapM_ putStrLn