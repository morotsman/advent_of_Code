{-# LANGUAGE ScopedTypeVariables #-}
module Day (scoreRecursiveGame, Deck, RecursivePlayer(..), getRecursivePlayers, playGameRecursiveCombat, parseRows, scoreGame, playGame, getPlayers, Player(..)) where

import qualified Data.Set as Set
import Debug.Trace (trace)

type Deck = [Int]

data Player = Player {
  name :: String,
  deck :: Deck
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
  (newDeck1, newDeck2) = playRound' deck1 deck2
  in (Player name1 newDeck1, Player name2 newDeck2)

playRound' :: Deck -> Deck -> (Deck, Deck)
playRound' deck1 deck2 = let
  card1 = head deck1
  card2 = head deck2
  in if card1 > card2
    then (tail deck1 ++ [card1, card2], tail deck2)
    else (tail deck1, tail deck2 ++ [card2, card1])

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

data RecursivePlayer = RecursivePlayer {
  decks :: [Deck]
} deriving (Show, Eq)

getRecursivePlayers :: [String] -> (RecursivePlayer, RecursivePlayer)
getRecursivePlayers rows = let
  players = parseRows rows
  player1 = head players
  player2 = last players
  in (RecursivePlayer [deck player1], RecursivePlayer [deck player2])

playGameRecursiveCombat :: RecursivePlayer -> RecursivePlayer -> (RecursivePlayer, RecursivePlayer)
playGameRecursiveCombat player1@(RecursivePlayer decks1) player2@(RecursivePlayer decks2)
  | length (head decks1) == 0 || length (head decks2) == 0 = (player1, player2)
  | elem (head decks1) (tail decks1) || elem (head decks2) (tail decks2) = (player1, RecursivePlayer []) -- player 1 winner if deck already present
  | otherwise = let
    (newPlayer1, newPlayer2) = playRoundRecursiveCombat player1 player2
    in  playGameRecursiveCombat newPlayer1 newPlayer2

playRoundRecursiveCombat :: RecursivePlayer -> RecursivePlayer -> (RecursivePlayer, RecursivePlayer)
playRoundRecursiveCombat (RecursivePlayer decks1) (RecursivePlayer decks2) = let
  deck1 = head decks1
  deck2 = head decks2
  card1 = head deck1
  card2 = head deck2
  in
    --trace ("Player 1 deck: " ++ show deck1 ++ " Player 1 decks: " ++ show decks1) $
    --trace ("Player 2 deck: " ++ show deck2 ++ " Player 2 decks: " ++ show decks2) $
    if (card1 <= (length $ tail deck1) && card2 <= (length $ tail deck2))
    then let
      (newPlayer1, newPlayer2) = playGameRecursiveCombat (RecursivePlayer [tail deck1]) (RecursivePlayer [tail deck2])
      hasPlayer1Won = length (head (decks newPlayer1)) > 0
      in
        --trace("playRoundRecursiveCombat1") $
        if hasPlayer1Won
          then (RecursivePlayer((tail deck1 ++ [card1, card2]):decks1), RecursivePlayer((tail deck2):decks2))
          else (RecursivePlayer((tail deck1):decks1), RecursivePlayer((tail deck2 ++ [card2, card1]):decks2))
    else let
      (newDeck1, newDeck2) = playRound' deck1 deck2
      in
        --trace("playRoundRecursiveCombat2")
        (RecursivePlayer (newDeck1 : decks1), RecursivePlayer (newDeck2 : decks2))


scoreRecursiveGame :: (RecursivePlayer, RecursivePlayer) -> Int
scoreRecursiveGame (player1, player2) = let
  winningDeck = head $ decks $ winningRecursivePlayer (player1, player2)
  in foldl (\acc (multiplier, card) -> acc + multiplier * card) 0 $ zip [1..] $ reverse winningDeck

winningRecursivePlayer :: (RecursivePlayer, RecursivePlayer) -> RecursivePlayer
winningRecursivePlayer (RecursivePlayer decks1, RecursivePlayer decks2) = if length (head decks1) == 0
  then RecursivePlayer decks2
  else RecursivePlayer decks1