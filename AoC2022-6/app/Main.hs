module Main where

import System.Environment
import Data.List (nub, scanl')
import Debug.Trace
import Text.Printf
import Control.Exception
import System.CPUTime

time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

main = do
  file <- readFile "compressed2.txt"
  putStrLn "Starting..."
  time $ analyze1 14 file `seq` return ()
  time $ analyze2 14 file `seq` return ()
  putStrLn "Done."
  -- print $ (analyze1 14 file)

analyze1::Int->String->Int
analyze1 windowSize stream = length $ substring stream
        where substring str =
                let window = take windowSize str
                in  if windowSize == (length . nub $ window) then window++[] else (head str):substring (drop 1 str)

analyze2::Int->String->Int
analyze2 windowSize stream =
        let calcNewAcc (_,oldWindow) newValue =
              let newWindow = (drop 1 oldWindow) ++ [newValue]
              in  if windowSize /= (length . nub $ newWindow)
                  then (1, newWindow)
                  else (0, newWindow)
            (firstWindow, rest) = splitAt windowSize stream
        in (+) windowSize $ length . takeWhile (\(i,_)-> i==1) $ scanl' calcNewAcc (1,firstWindow) rest
