{-# LANGUAGE ScopedTypeVariables #-}
module Day (scoreGame, playGame, getPlayers, Player(..)) where

data Player = Player {
  name :: String,
  deck :: [Int]
} deriving (Show, Eq)

getPlayers :: [String] -> (Player, Player)
getPlayers rows = let
  players = parseRows rows
  in (head players, last players)

parseRows :: [String] -> [Player]
parseRows rows = let
  playerRows = groupBy "" rows
  in map parsePlayer playerRows

parsePlayer :: [String] -> Player
parsePlayer (name:cards) = Player name (map read cards)

groupBy :: Eq a => a -> [a] -> [[a]]
groupBy discriminator rows = let
  isDiscriminator = (==) discriminator
  in foldl (\acc x -> if isDiscriminator x
    then acc ++ [[]]
    else (init acc) ++ [(last acc) ++ [x]]) [[]] rows

playRound :: (Player, Player) -> (Player, Player)
playRound (Player name1 deck1, Player name2 deck2) = let
  card1 = head deck1
  card2 = head deck2
  in if card1 > card2
    then (Player name1 (tail deck1 ++ [card1, card2]), Player "" (tail deck2))
    else (Player name2 (tail deck1), Player "" (tail deck2 ++ [card2, card1]))

playGame :: (Player, Player) -> (Player, Player)
playGame players@(Player _ deck1, Player _ deck2) | length deck1 == 0 || length deck2 == 0 = players
playGame players = playGame (playRound players)

winningPlayer :: (Player, Player) -> Player
winningPlayer (Player name1 deck1, Player name2 deck2) = if length deck1 == 0
  then Player name2 deck2
  else Player name1 deck1

scoreGame :: (Player, Player) -> Int
scoreGame (player1, player2) = let
  winningDeck = deck $ winningPlayer (player1, player2)
  in foldl (\acc (multiplier, card) -> acc + multiplier * card) 0 $ zip [1..] $ reverse winningDeck
