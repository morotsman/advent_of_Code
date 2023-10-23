module Main where

-- A == Rock
-- B == Paper
-- C == Scissors

-- X == Rock
-- Y == Paper
-- Z == Scissors

resultOfRound :: String -> Int
resultOfRound row | row == "A Y" = 6 -- win
                  | row == "A X" = 3 -- draw
                  | row == "A Z" = 0 -- loss
                  | row == "B X" = 0 -- loss
                  | row == "B Y" = 3 -- draw
                  | row == "B Z" = 6 -- win
                  | row == "C X" = 6 -- win
                  | row == "C Y" = 0 -- loss
                  | row == "C Z" = 3 -- draw


containsCharacter :: Char -> String -> Bool
containsCharacter char str = char `elem` str

resultOfChoice :: String -> Int
resultOfChoice row | containsCharacter 'X' row = 1
                   | containsCharacter 'Y' row = 2
                   | containsCharacter 'Z' row = 3

score :: String -> Int
score row = resultOfRound(row) + resultOfChoice(row)


main :: IO ()
main = do
  content <- readFile "/Users/nikleo/workspace/advent/2022/app/Day2_input.txt"
  let linesOfFile = lines content
  let result = (sum . map score) linesOfFile
  print result
