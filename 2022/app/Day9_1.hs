{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens
import Data.Set (Set, fromList)

type Answer =  Maybe Int
data Coordinate = Coordinate(Int, Int) deriving (Show, Eq, Ord)

data Movement = MoveUp
                | MoveDown
                | MoveLeft
                | MoveRight
                deriving (Show, Eq)

data Rope = Rope(Coordinate, Coordinate) deriving (Show)

type VisitedCoordinates = [Rope]

type Distance = Int

solveIt :: [String] -> Answer
solveIt input = do
  let rope = Rope(Coordinate(0,0), Coordinate(0,0))
  movements <- traverse parseRow input
  let visitedCoordinates = foldl (\ vc@(r:rs) movement ->  (reverse (moveRope r movement)) ++ vc ) [rope] movements
  let tailPosition = fmap (\ (Rope(head, tail)) -> tail) visitedCoordinates
  let uniqueTailPositions = fromList tailPosition
  return (length uniqueTailPositions)

parseRow :: String -> Maybe (Movement, Distance)
parseRow input | stringBeginningWith "R " input = fmap (\d -> (MoveRight, d)) (getDistance input)
               | stringBeginningWith "L " input = fmap (\d -> (MoveLeft, d)) (getDistance input)
               | stringBeginningWith "U " input = fmap (\d -> (MoveUp, d)) (getDistance input)
               | stringBeginningWith "D " input = fmap (\d -> (MoveDown, d)) (getDistance input)

stringBeginningWith :: String -> String -> Bool
stringBeginningWith beginning string = beginning == take (length beginning) string

getDistance :: String -> Maybe Int
getDistance row = fmap read (words row) ^? element 1

moveRope :: Rope -> (Movement, Distance) -> [Rope]
moveRope (Rope(head,tail)) (_, distance) | distance == 0 = []
moveRope (Rope(Coordinate(hx, hy), Coordinate(tx, ty))) (movement, distance) =
  let headMovement = moveHead (Coordinate(hx, hy)) movement
      tailMovement = moveTail headMovement (Coordinate(tx, ty))
      rope = Rope(headMovement, tailMovement)
  in [rope] ++ moveRope rope (movement, (distance - 1))

moveHead :: Coordinate -> Movement -> Coordinate
moveHead (Coordinate(x, y)) movement | movement == MoveRight = Coordinate(x + 1, y)
                                     | movement == MoveLeft = Coordinate(x - 1, y)
                                     | movement == MoveUp = Coordinate(x, y + 1)
                                     | movement == MoveDown = Coordinate(x, y - 1)

moveTail :: Coordinate -> Coordinate -> Coordinate
moveTail head@(Coordinate(hx, hy)) tail@(Coordinate(tx, ty)) | isTouching head tail = (Coordinate(tx, ty))
                                                             | moveRight head tail = (Coordinate(tx + 1, ty))
                                                             | moveLeft head tail = (Coordinate(tx - 1, ty))
                                                             | moveUp head tail = (Coordinate(tx, ty + 1))
                                                             | moveDown head tail  = (Coordinate(tx, ty - 1))
                                                             | otherwise = moveDiagonally head tail

isTouching :: Coordinate -> Coordinate -> Bool
isTouching head@(Coordinate(hx, hy)) tail@(Coordinate(tx, ty)) = head == tail || ((abs (hx - tx) < 2) && (abs (hy - ty) < 2))

moveRight :: Coordinate -> Coordinate -> Bool
moveRight (Coordinate(hx, hy)) (Coordinate(tx, ty)) = hx - tx > 1 && hy == ty

moveLeft :: Coordinate -> Coordinate -> Bool
moveLeft (Coordinate(hx, hy)) (Coordinate(tx, ty)) = hx - tx < - 1 && hy == ty

moveUp :: Coordinate -> Coordinate -> Bool
moveUp (Coordinate(hx, hy)) (Coordinate(tx, ty)) = hx == tx && hy - ty > 1

moveDown :: Coordinate -> Coordinate -> Bool
moveDown (Coordinate(hx, hy)) (Coordinate(tx, ty)) = hx == tx && hy - ty < -1

moveDiagonally :: Coordinate -> Coordinate -> Coordinate
moveDiagonally head@(Coordinate(hx, hy)) tail@(Coordinate(tx, ty)) | hx > tx && hy > ty = (Coordinate(tx + 1, ty + 1))
                                                                   | hx < tx && hy > ty = (Coordinate(tx - 1, ty + 1))
                                                                   | hx > tx && hy < ty = (Coordinate(tx + 1, ty - 1))
                                                                   | hx < tx && hy < ty = (Coordinate(tx - 1, ty - 1))

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day9_input.txt"
  let linesOfFile = lines content
  let answer = solveIt linesOfFile
  print answer
