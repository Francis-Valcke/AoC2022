module Main where

import System.Environment
import Data.List (nub)
import Debug.Trace

main = do
  file <- readFile "input1b.txt"
  print $ (analyze 14 file)


analyze::Int->String->Int
analyze windowSize stream = length $ substring stream
        where substring str =
                let window = take windowSize str
                in  if windowSize == (length . nub $ window) then window++[] else (head str):substring (drop 1 str)


-- Meh
-- analyze::Int->String->Int
-- analyze windowSize stream =
--                       let calcNewAcc (_,oldWindow) newValue =
--                                         let newWindoww = (drop 1 oldWindow) ++ [newValue]
--                                             newWindow = trace newWindoww newWindoww
--                                         in  if windowSize /= (length . nub $ newWindow)
--                                             then (1, newWindow)
--                                             else (0, newWindow)
--                           (firstWindow, rest) = splitAt windowSize stream
--                           in (+) 4 $ length . takeWhile (\(i,_)-> i==1) $ scanl calcNewAcc (1,firstWindow) rest



