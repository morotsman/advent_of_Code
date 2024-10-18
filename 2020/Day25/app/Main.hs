module Main where

import Challenge

main :: IO ()
main = do
  -- Extract the result from transformSubjectNumber (ignoring the cache)
  let (result1, _) = transformSubjectNumber 8 7 mempty
  putStrLn $ show result1

  putStrLn $ show $ getLoopNumber 5764801 7
  putStrLn $ show $ getLoopNumber 17807724 7

  let (result2, _) = transformSubjectNumber 8 17807724 mempty
  putStrLn $ show result2

  putStrLn $ show $ getLoopNumber 1614360 7
  putStrLn $ show $ getLoopNumber 7734663 7

  let (result3, _) = transformSubjectNumber 1182212 7734663 mempty
  putStrLn $ show result3
