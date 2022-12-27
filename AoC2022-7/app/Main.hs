module Main where
import Data.Char (isNumber)
import Debug.Trace
import Text.Printf
import Data.Char
import Data.List (foldl', isPrefixOf, nub, sort)

main :: IO ()
main = do
  file <- readFile $ "input1b.txt"
  let inputLines = lines file
  let associated = associateWithPath inputLines []
  let invertedPaths = invertPaths associated
  let dirs = distinctPaths invertedPaths
  let filteredFiles = filterFiles invertedPaths
  let dirSizes = calcSizes dirs filteredFiles
  -- Part 1
  let part1 = sum . map (\(_,size) -> size) $ filter (\(dir, size) -> size <= 100000) dirSizes
  putStrLn $ "Part1: " ++ show part1
  -- Part 2
  let rootDirSize = snd . head $ dirSizes
  let sizeToBeDeleted = 30000000 - (70000000 - rootDirSize)
  let extractedDirSizes = map (\(_,size)-> size) dirSizes
  let sizeToDelete = head . filter (\dirSize -> dirSize > sizeToBeDeleted) $ sort extractedDirSizes
  putStrLn $ "Part2: " ++ show sizeToDelete

calcSizes::[[String]]->[(Int, [String])]->[([String],Int)]
calcSizes dirs files = map (\dirPath -> (dirPath,calcSize dirPath files)) dirs
                where calcSize::[String]->[(Int, [String])]->Int
                      calcSize path files = sum . map (\(size,_)->size) $ filter (\(size, filePath) -> path `isPrefixOf` filePath) files

distinctPaths::[(a, [String])]->[[String]]
distinctPaths input =
                let paths = map (\(_,x)->x) input
                in nub paths

invertPaths::[(a, [String])]->[(a, [String])]
invertPaths xs = map invertPath xs
                where invertPath (f,path) = (f, reverse path)

filterFiles::[(String, [String])]-> [(Int, [String])]
filterFiles associatedPaths = map toNum $ filter isFile associatedPaths
              where isFile (f, _) = isDigit . head . head . words $ f
                    toNum (f, xs) = (read . head. words $ f, xs)

associateWithPath::[String]->[String]->[(String, [String])]
associateWithPath (line:rest) currPath =
                      let newPath = calcPath line currPath
                      in (line, newPath):(associateWithPath rest newPath)
associateWithPath (line:[]) currPath =
                      let newPath = calcPath line currPath
                      in (line, newPath):[]
associateWithPath [] currPath = [("nope", ["nope"])]

calcPath::String->[String]->[String]
calcPath line currPath
            | "$ cd " `isPrefixOf` line =
                      let path = drop 5 line
                      in case path of ".." -> drop 1 currPath
                                      xs -> xs : currPath
            | otherwise = currPath
