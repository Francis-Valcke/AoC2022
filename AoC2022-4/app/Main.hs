module Main where
import System.IO
import Control.Monad
import qualified Data.List.Split as Split
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Char(digitToInt)

main :: IO ()
main = do
  input <- readFile "input1b.txt"
  let split = map (Split.splitOn ",") $ words input
  let bounds = map (toTuple . (map parseRange)) split
  -- ex1
  let countContained = count . map contained $ bounds
  print countContained
  -- ex2
  let countContained = count . map overlaps $ bounds
  print countContained

parseRange::String->(Int, Int)
parseRange e =
  let split = Split.splitOn "-" e
      lower = read (head split)::Int
      upper = read (last split)::Int
  in (lower, upper)

toTuple::[(Int,Int)] -> ((Int,Int),(Int,Int))
toTuple a = ((head a), (last a))

contained::((Int, Int),(Int, Int)) -> Bool
contained (fst,snd) = fst `contains` snd || snd `contains` fst
      where contains (a,b) (x,y) = a <= x && b >= y

overlaps::((Int, Int),(Int, Int)) -> Bool
overlaps ((a,b),(x,y)) = not (a > y || b < x)

count::[Bool]->Int
count = sum . map (\e -> if e == True then 1 else 0)
