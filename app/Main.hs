module Main where
import System.IO
import Control.Monad
import qualified Data.List.Split as L
import qualified Data.List as DL

main :: IO ()
main = do
  let list = []
  handle <- openFile "input1b.txt" ReadMode
  contents <- hGetContents handle
  let split = L.splitOn "\n\n" contents
  let sumsList = sums . toInts . toNestedArrays $ split
  let max = maximum sumsList
  let indexed = index $ sumsList
  -- a
  print . getMax 1 $ indexed
  -- b
  let maxTups = getMax 3 $ indexed
  let sumTot = sum . map (\(a,b) -> b) $ maxTups
  print sumTot
  hClose handle

toNestedArrays :: [String] -> [[String]]
toNestedArrays = map words

toInts :: [[String]] -> [[Int]]
toInts input = map (\b -> map (\a -> read a::Int) b) input

sums :: [[Int]] -> [Int]
sums input = map (foldl1 (+)) input

index :: [Int] -> [(Int, Int)]
index = zip [1..]

getMax :: Int -> [(Int, Int)] -> [(Int, Int)]
getMax cnt tups = take cnt $ DL.sortBy (\(a,b) (x,y) -> compare y b) $ tups
