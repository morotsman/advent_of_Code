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

type Rope = [Coordinate]

type Distance = Int

solveIt :: [String] -> Answer
solveIt input = do
  let rope = [Coordinate(0,0), Coordinate(0,0), Coordinate(0,0), Coordinate(0,0), Coordinate(0,0), Coordinate(0,0), Coordinate(0,0), Coordinate(0,0), Coordinate(0,0), Coordinate(0,0)]
  movements <- traverse parseRow input
  let ropePositions :: [Rope] = foldl (\ vc@(r:rs) movement ->  (reverse (moveRope r movement)) ++ vc ) [rope] movements
  let tailPosition = fmap (\ rope -> rope !! 9) ropePositions
  let uniqueTailPositions = fromList tailPosition
  return (length uniqueTailPositions)
  --return (reverse ropePositions)

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
moveRope _ (_, distance) | distance == 0 = []
moveRope [] _ = []
moveRope (head:tail) (movement, distance) =
  let (Coordinate(hx, hy)) = head
      headMovement = moveHead (Coordinate(hx, hy)) movement
      rope = [headMovement] ++ moveTails tail headMovement movement
  in [rope] ++ moveRope rope (movement, (distance - 1))

moveTails :: Rope -> Coordinate -> Movement -> Rope
moveTails [] _ _ = []
moveTails (r1:rs) coordinate movement =
  let tailMovement = moveTail coordinate r1
  in [tailMovement] ++ moveTails rs tailMovement movement

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
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day9_input.txt" -- 5874 / 13
  let linesOfFile = lines content
  let answer = solveIt linesOfFile
  print answer
