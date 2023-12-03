{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List (isInfixOf)
import Control.Lens

data Row = DirectoryRow String
            | FileRow String Int
            | CDRow String
            | LSRow
            deriving (Show)

data FileTree = Directory String [FileTree]
                | File String Int
                deriving (Show)

type Path = [String]

solveIt :: [String] -> Maybe (FileTree, Path)
solveIt inputs = do
  parsedRows <- traverse parseRow inputs
  let fileTree = buildFileTree parsedRows
  return fileTree

parseRow :: String -> Maybe Row
parseRow input | isInfixOf "cd" input = parseCD input
               | isInfixOf "ls" input = Just(LSRow)
               | isInfixOf "dir" input = parseDir input
               | otherwise = parseFile input

parseCD :: String -> Maybe Row
parseCD input = do
  let columns = words input
  moveTo <- columns ^? element 2
  return (CDRow moveTo)

parseDir :: String -> Maybe Row
parseDir input = do
  let columns = words input
  name <- columns ^? element 1
  return (DirectoryRow name)

parseFile :: String -> Maybe Row
parseFile input = do
  let columns = words input
  size <- fmap read columns ^? element 0
  name <- columns ^? element 1
  return (FileRow name size)

buildFileTree :: [Row] -> (FileTree, Path)
buildFileTree rows = foldl buildFileTree' (Directory "/" [], []) rows

buildFileTree' ::  (FileTree, Path) -> Row -> (FileTree, Path)
buildFileTree' (fileTree, (p:ps)) (CDRow moveTo) | moveTo == ".." = (fileTree, ps)
buildFileTree' (fileTree, path) (CDRow moveTo) = (fileTree, [moveTo] ++ path)
buildFileTree' (fileTree, path) (LSRow) = (fileTree, path)
buildFileTree' (fileTree, p:ps) row = (updateFileTree fileTree p row, [p] ++ ps)

updateFileTree :: FileTree -> String -> Row -> FileTree
updateFileTree file@(File name size) _ _ = file
updateFileTree (Directory name fileTrees) directoryToUpdate (DirectoryRow dirName) | name == directoryToUpdate =
  Directory name (fileTrees ++ [Directory dirName []])
updateFileTree (Directory name fileTrees) directoryToUpdate (FileRow dirName size) | name == directoryToUpdate =
  Directory name (fileTrees ++ [File dirName size])
updateFileTree (Directory name fileTrees) directoryToUpdate row = do
  let updatedFileTrees = fmap (\fileTree -> updateFileTree fileTree directoryToUpdate row) fileTrees
  Directory name updatedFileTrees

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day7_example.txt"
  let linesOfFile = lines content
  print $ solveIt linesOfFile
