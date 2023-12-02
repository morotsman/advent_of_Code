{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (isInfixOf)
import Control.Lens

data FileSystem = Directory String
            | File String Int
            deriving (Show)

data Command = CD String
            | LS
            deriving (Show)

data Row = FileSystemRow FileSystem
            | CommandRow Command
            deriving (Show)


solveIt :: [String] -> Maybe [Row]
solveIt inputs = do
  --let root = Directory "Root" []
  let parsedRows :: Maybe [Row] = sequenceA $ fmap parseRow inputs
  parsedRows

parseRow :: String -> Maybe Row
parseRow input | isInfixOf "cd" input = parseCD input
               | isInfixOf "ls" input = Just(CommandRow (LS))
               | isInfixOf "dir" input = parseDir input
               | otherwise = parseFile input

parseCD :: String -> Maybe Row
parseCD input = do
  let columns = words input
  moveTo <- columns ^? element 2
  return (CommandRow (CD moveTo))

parseDir :: String -> Maybe Row
parseDir input = do
  let columns = words input
  name <- columns ^? element 1
  return (FileSystemRow (Directory name))

parseFile :: String -> Maybe Row
parseFile input = do
  let columns = words input
  size <- fmap read columns ^? element 0
  name <- columns ^? element 1
  return (FileSystemRow (File name size))

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day7_example.txt"
  let linesOfFile = lines content
  print $ solveIt linesOfFile
