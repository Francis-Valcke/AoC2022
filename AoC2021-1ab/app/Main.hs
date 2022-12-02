module Main where
import System.IO
import Control.Monad

main = do
        let list = []
        handle <- openFile "input2b.txt" ReadMode
        contents <- hGetContents handle
        let input = readAsNumbers . words $ contents
        -- ex 1
        -- let shifted = -1 : input
        -- let tups = zip shifted input
        -- print . length $ filter (True&&) $ determine $ tups
        -- ex 2
        let tuples = customZip input
        print tuples
        let sums = tupleSum tuples
        print sums
        let shifted = -1 : sums
        let tups = zip shifted sums
        print . length $ filter (True&&) $ determine $ tups

        hClose handle

readAsNumbers :: [String] -> [Int]
readAsNumbers input = map read $ input

tuples :: [Int] -> [(Int, Int)]
tuples input = zip [1..] input

determine :: [(Int, Int)] -> [Bool]
determine xs = tail . map (\(a,b) -> b > a) $ xs

tupleSum :: [(Int, Int, Int)] -> [Int]
tupleSum xs = map (\(a,b,c) -> a + b + c) xs

customZip :: [Int] -> [(Int, Int, Int)]
customZip (a:b:c:[]) = (a,b,c):[]
customZip (a:b:c:xs) = (a,b,c) : (customZip $ b:c:xs)

-- determine :: [Int] -> [String]
-- determine (x:_) = "(N/A - no previous measurement)"
-- determine (_:rest) =
