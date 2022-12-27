module Main where
import Data.Char (isNumber)
import Debug.Trace
import Text.Printf
import Data.List (foldl', isPrefixOf)

main :: IO ()
main = do
  file <- readFile $ "input1a.txt"
  let inputLines = lines file
  let test = evaluateLine inputLines [] 0
  print $ takeScope inputLines [] []


-- rest of file -> current path -> skipLines -> size
evaluateDir::[String]->[String]-> Int
evaluateDir restOfFile path = 0
    -- let lineToEvaluate = head $ drop skipLines file
    -- in parse lineToEvaluate
    -- where parse line
    --         | "$ cd" `isPrefixOf` line =
    --                       let target = drop 4 line
    --                       in case target of ".." ->
    --                                         other ->
    --         | "$ ls" `isPrefixOf` line = evaluateLine rest currPath counter
    --         | "dir" `isPrefixOf` line = evaluateLine rest currPath counter
    --         | otherwise =
-- evaluateLine (line:[]) currPath counter = read . head . words $ line

-- file -> prefix -> currentPath ->
takeScope::[String]->[String] -> [String]->[String]
takeScope (line:rest) pathPrefix path =
              let newPath = calcNewPath
              in if pathPrefix == (take (length pathPrefix) [])
                 then line : takeScope rest pathPrefix newPath
                 else line : []
              where calcNewPath
                      | "$ cd" `isPrefixOf` line =
                                    let target = drop 4 line
                                    in case target of ".." -> drop 1 path
                                                      other -> other : path
                      | otherwise = path
