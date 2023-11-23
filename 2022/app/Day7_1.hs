{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (isInfixOf)
import Control.Lens

data FileSystem = Directory String [FileSystem]
            | File String Int
            deriving (Show)

data Command = CD String
            | LS
            deriving (Show)

data Row = FileSystemRow FileSystem
            | CommandRow Command
            deriving (Show)


solveIt :: [String] -> Maybe(FileSystem, Int, [Row])
solveIt inputs = do
  let root = Directory "Root" []
  parsedRows <- sequenceA $ fmap parseRow inputs
  let fileSystem = buildFileSystem parsedRows root
  let sizes = calculateSizes fileSystem
  return (fileSystem, summarize sizes, parsedRows)

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
  return (FileSystemRow (Directory name []))

parseFile :: String -> Maybe Row
parseFile input = do
  let columns = words input
  size <- fmap read columns ^? element 0
  name <- columns ^? element 1
  return (FileSystemRow (File name size))


buildFileSystem :: [Row] -> FileSystem -> FileSystem
buildFileSystem inputs filesystem = Directory "one" [File "test" 10, Directory "two" []]

calculateSizes :: FileSystem -> [Int]
calculateSizes fileSystem = [42]

summarize :: [Int] -> Int
summarize sizes = sum sizes

main :: IO ()
main = do
  content <- readFile "/Users/nikleo/workspace/advent_of_Code/2022/app/Day7_example.txt"
  let linesOfFile = lines content
  print $ solveIt linesOfFile
