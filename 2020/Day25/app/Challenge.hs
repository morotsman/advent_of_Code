{-# LANGUAGE ScopedTypeVariables #-}
module Challenge (transformSubjectNumber, getLoopNumber) where

import qualified Data.Map as Map
import Debug.Trace (trace)

type MemoCache = Map.Map Integer Integer

transformSubjectNumber :: Integer -> Integer -> MemoCache -> (Integer, MemoCache)
transformSubjectNumber 0 subjectNumber cache = (subjectNumber, cache)
transformSubjectNumber loopSize subjectNumber cache =
    case Map.lookup loopSize cache of
        Just result -> (result, cache)
        Nothing     ->
            let (nextResult, newCache) = transform (loopSize - 1) subjectNumber cache
                result = rem (nextResult * subjectNumber) 20201227
            in (result, Map.insert loopSize result newCache)
  where
    transform :: Integer -> Integer -> MemoCache -> (Integer, MemoCache)
    transform 0 _ cache = (1, cache)
    transform loopSize subjectNumber cache =
        case Map.lookup loopSize cache of
            Just value -> (value, cache)
            Nothing ->
                let (nextValue, updatedCache) = transform (loopSize - 1) subjectNumber cache
                    newValue = rem (nextValue * subjectNumber) 20201227
                in (newValue, Map.insert loopSize newValue updatedCache)

getLoopNumber :: Integer -> Integer -> Integer
getLoopNumber publicKey subjectNumber =
    getLoopNumber' 1 Map.empty
  where
    getLoopNumber' :: Integer -> MemoCache -> Integer
    getLoopNumber' loopSize cache =
        let (transformed, newCache) = transformSubjectNumber loopSize subjectNumber cache
        in if transformed == publicKey
            then loopSize
            else getLoopNumber' (loopSize + 1) newCache
