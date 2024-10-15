{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Day2 (playGame2)

main :: IO ()
main = do
    putStrLn "hello"

    result <- playGame2 100 ([7, 9, 2, 8, 4, 5, 1, 3, 6])
    putStrLn $ show result

