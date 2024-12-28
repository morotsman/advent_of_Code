{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Debug.Trace
import Data.IORef
import MatrixUtil (Matrix(..), Coordinate( .. ))

data Content = Rock | Air | Sand deriving (Eq)

--data Line = {
--    from ::
--}

instance Show Content where
  show Rock = "#"
  show Air  = "."
  show Sand = "*"

newtype ShapeOfRock = ShapeOfRock (Matrix (IORef Content)) deriving (Eq)

showShapeOfRock :: ShapeOfRock -> IO String
showShapeOfRock (ShapeOfRock (Matrix rows)) = do
  contents <- mapM (mapM readIORef) rows
  return $ unlines $ map (concatMap show) contents

solveIt :: [String] -> [String]
solveIt input = input

--formShapeOfRock :: [String] -> ShapeOfRock
--formShapeOfRock input =

main :: IO ()
main = do
  -- Create a sample ShapeOfRock
  rock <- newIORef Rock
  air <- newIORef Air
  sand <- newIORef Sand

  let shape = ShapeOfRock (Matrix [[rock, air], [sand, rock]])

  -- Show the ShapeOfRock contents
  putStrLn =<< showShapeOfRock shape
