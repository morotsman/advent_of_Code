{-# LANGUAGE ScopedTypeVariables #-}
module Day (indexesToPickUp, pickUp, selectDestinationCup, playGame) where

import qualified Data.Set as Set
import Debug.Trace (trace)

type Cups = [Int]
type CurrentCup = Int
type Round = Int

playGame :: Round -> Int -> Int
playGame rounds input = let
  cups = map (\x -> read [x]) (show input)
  result = playGame' rounds (0, cups)
  in read (concatMap show (snd result))

playGame' :: Round -> (CurrentCup, Cups) -> (CurrentCup, Cups)
playGame' rounds (currentCup, cups) | rounds == 0 = (currentCup, cups)
                                    | otherwise = playGame' (rounds - 1) (playRound (currentCup, cups))

playRound :: (CurrentCup, Cups) -> (CurrentCup, Cups)
playRound (currentCup, cups) = let
  (pickedUpCups, leftovers) = pickUp 3 currentCup cups
  destinationCup = selectDestinationCup (cups !! currentCup) pickedUpCups leftovers
  newCups = insertCups currentCup destinationCup pickedUpCups leftovers
  in (if (currentCup >= (length cups - 1 )) then 0 else currentCup + 1, newCups)

insertCups :: CurrentCup -> CurrentCup -> Cups -> Cups -> Cups
insertCups currentCup destinationCup pickedUpCups leftovers = let
  beforeDestinationCup = takeWhile (\c -> not (c == destinationCup)) leftovers
  afterDestinationCup = drop 1 $ dropWhile (\c -> not (c == destinationCup)) leftovers
  newCups = beforeDestinationCup ++ [destinationCup] ++ pickedUpCups ++ afterDestinationCup
  afterCurrentCup = take (length newCups - currentCup) newCups
  beforeCurrentCup = drop (length newCups - currentCup) newCups
  in beforeCurrentCup ++ afterCurrentCup

pickUp :: Int -> CurrentCup -> Cups -> (Cups, Cups)
pickUp numberOfCups currentCup cups = let
  indexesToPick = indexesToPickUp numberOfCups currentCup cups
  pickedUp = fmap (cups !!) indexesToPick
  pre = filter (\cup -> not (elem cup pickedUp)) $ take currentCup cups
  post = drop (length pre + 1) $ removeIndexes indexesToPick cups
  leftovers =  [cups !! currentCup] ++ post ++ pre
  in (pickedUp, leftovers)

removeIndexes :: [Int] -> Cups -> Cups
removeIndexes indexes cups = fmap snd $ filter (\(i, _) -> not (elem i indexes)) (zip [0..] cups)

indexesToPickUp :: Int -> CurrentCup -> Cups -> [Int]
indexesToPickUp numberOfCups currentCup cups = take numberOfCups $ fmap (\i -> mod i (length cups)) [(currentCup+1)..]

selectDestinationCup :: CurrentCup -> Cups -> Cups -> CurrentCup
selectDestinationCup currentCup pickedUpCups leftovers
  | ((currentCup - 1) < minimum (pickedUpCups ++ leftovers)) = selectDestinationCup (maximum (pickedUpCups ++ leftovers) + 1) pickedUpCups leftovers
  | not $ elem (currentCup - 1)  pickedUpCups = currentCup - 1
  | otherwise = selectDestinationCup (currentCup - 1) pickedUpCups leftovers