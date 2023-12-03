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

solveIt :: [String] -> Maybe Int
solveIt inputs = do
  parsedRows <- traverse parseRow inputs
  let (fileTree, path) = buildFileTree parsedRows
  let directorySizes = calculateDirSizes fileTree
  let answer = sum (filter (\size -> size <= 100000) directorySizes)
  return answer

calculateDirSizes :: FileTree -> [Int]
calculateDirSizes (File name size) = []
calculateDirSizes dir@(Directory name fileTrees) = do
  let subDirectories = concatMap calculateDirSizes fileTrees
  [calculateDirSize dir] ++ subDirectories

calculateDirSize :: FileTree -> Int
calculateDirSize (File name size) = size
calculateDirSize (Directory name fileTrees) = do
  let subTrees :: [Int] = fmap calculateDirSize fileTrees
  sum subTrees

parseRow :: String -> Maybe Row
parseRow input | stringBeginningWith "$ cd" input = parseCD input
               | "$ ls" == input = Just(LSRow)
               | stringBeginningWith "dir" input = parseDir input
               | otherwise = parseFile input

stringBeginningWith :: String -> String -> Bool
stringBeginningWith beginning string = beginning == take (length beginning) string

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
buildFileTree' (fileTree, p:ps) row = (updateFileTree p row fileTree, [p] ++ ps)

updateFileTree ::  String -> Row -> FileTree -> FileTree
updateFileTree _ _ file@(File name size) = file
updateFileTree directoryToUpdate (DirectoryRow dirName) (Directory name fileTrees) | name == directoryToUpdate =
  Directory name (fileTrees ++ [Directory dirName []])
updateFileTree directoryToUpdate (FileRow fileName size) (Directory name fileTrees) | name == directoryToUpdate =
  Directory name (fileTrees ++ [File fileName size])
updateFileTree directoryToUpdate row (Directory name fileTrees) = do
  let updatedFileTrees = fmap (updateFileTree directoryToUpdate row) fileTrees
  Directory name updatedFileTrees

main :: IO ()
main = do
  content <- readFile "/Users/niklasleopold/workspace/advent_of_Code/2022/app/Day7_example.txt"
  let linesOfFile = lines content
  print $ solveIt linesOfFile
