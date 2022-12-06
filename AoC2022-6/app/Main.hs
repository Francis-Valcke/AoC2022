module Main where

import System.Environment
import Data.List (nub)
import Debug.Trace

main = do
  file <- readFile "input1b.txt"
  print $ (analyze file 14)

analyze::String->Int->Int
analyze stream windowSize = length $ substring stream
        where substring str =
                let window = take windowSize str
                in  if windowSize == (length . nub $ window) then window++[] else (head str):substring (drop 1 str)


