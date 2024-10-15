{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Day2 (playGame2)

main :: IO ()
main = do
    putStrLn "hello"

    result <- playGame2 10 ([3, 8,  9,  1,  2,  5,  4,  6,  7])
    putStrLn $ show result

