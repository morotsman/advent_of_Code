module Main where

-- A == Rock
-- B == Paper
-- C == Scissors

-- X == lose
-- Y == draw
-- Z == win


resultOfRound :: String -> Int
resultOfRound row  | containsCharacter 'X' row = 0
                   | containsCharacter 'Y' row = 3
                   | containsCharacter 'Z' row = 6


containsCharacter :: Char -> String -> Bool
containsCharacter char str = char `elem` str

toChoose :: String -> Char
toChoose row | containsCharacter 'A' row && containsCharacter 'X' row = 'C'
             | containsCharacter 'A' row && containsCharacter 'Y' row = 'A'
             | containsCharacter 'A' row && containsCharacter 'Z' row = 'B'
             | containsCharacter 'B' row && containsCharacter 'X' row = 'A'
             | containsCharacter 'B' row && containsCharacter 'Y' row = 'B'
             | containsCharacter 'B' row && containsCharacter 'Z' row = 'C'
             | containsCharacter 'C' row && containsCharacter 'X' row = 'B'
             | containsCharacter 'C' row && containsCharacter 'Y' row = 'C'
             | containsCharacter 'C' row && containsCharacter 'Z' row = 'A'

resultOfChoice :: Char -> Int
resultOfChoice choice | choice == 'A' = 1
                      | choice == 'B' = 2
                      | choice == 'C' = 3

score :: String -> Int
score row = resultOfRound row + (resultOfChoice . toChoose) row


main :: IO ()
main = do
  content <- readFile "/Users/nikleo/workspace/advent/2022/app/Day2_input.txt"
  let linesOfFile = lines content
  let result = (sum . map score) linesOfFile
  print result
