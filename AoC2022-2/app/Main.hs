module Main where
import System.IO
import Control.Monad
import qualified Data.List.Split as L
import qualified Data.List as DL

main :: IO ()
main = do
  handle <- openFile "input1b.txt" ReadMode
  contents <- hGetContents handle
  let split = words contents
  let pairs = toPairs $ split
  print pairs
  print . calcScore . map translateSymbol $ pairs
  print . calcScore . map translateSymbol2 $ pairs

calcScore::[(String,String)]->Int
calcScore = sum . map (\(a,b) -> (evaluateSymbol b) + (evaluatePair a b))

toPairs::[String]->[(String,String)]
toPairs (a:b:[]) = (a,b):[]
toPairs (a:b:xs) = (a,b):toPairs xs

translateSymbol::(String,String)->(String,String)
translateSymbol (a,b)
          | b == "X" = (a,"A")
          | b == "Y" = (a,"B")
          | b == "Z" = (a,"C")
          | otherwise = (a,"Q")

translateSymbol2::(String,String)->(String,String)
translateSymbol2 (a,b)
          | b == "X" && a == "A" = (a,"C")
          | b == "X" && a == "B" = (a,"A")
          | b == "X" && a == "C" = (a,"B")
          | b == "Y" = (a,a)
          | b == "Z" && a == "A" = (a,"B")
          | b == "Z" && a == "B" = (a,"C")
          | b == "Z" && a == "C" = (a,"A")
          | otherwise = (a,"Q")

evaluateSymbol::String->Int
evaluateSymbol symbol
          | symbol == "A" = 1
          | symbol == "B" = 2
          | symbol == "C" = 3
          | otherwise = -9999999

evaluatePair::String->String->Int
evaluatePair a b
          | a == b = 3
          | a == "A" && b == "B" = 6
          | a == "B" && b == "A" = 0
          | a == "A" && b == "C" = 0
          | a == "C" && b == "A" = 6
          | a == "B" && b == "C" = 6
          | a == "C" && b == "B" = 0
          | otherwise = -9999999

