module Main where
import System.IO
import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map

main :: IO ()
main = do

  handle <- openFile "input1b.txt" ReadMode
  contents <- hGetContents handle
  let backpacks = words contents

  let compartiments = map (split') $ backpacks

  let sharedTypes = map getSharedTypes $ compartiments

  -- ex 1
  print . sum $ map evaluate sharedTypes
  -- ex 2

  let grouped = group backpacks
  let badges = map (\(a,b,c) -> List.nub $ List.intersectBy (==) (List.intersectBy (==) a b) c)$ grouped
  print . sum $ map evaluate badges

group::[a]->[(a,a,a)]
group (a:b:c:[]) = (a,b,c):[]
group (a:b:c:xs) = (a,b,c):group xs

split'::String->(String,String)
split' e =
  let cutoff = div (length e) 2
      fst = take cutoff e
      snd = drop cutoff e
  in  (fst, snd)

evaluate::String->Int
evaluate shared = sum $ map lookupPrio shared

getSharedTypes::(String,String)->[Char]
getSharedTypes (a,b) = List.nub (List.intersectBy (==) a  b)

typePriority::Map.Map Char Int
typePriority = Map.fromList . zip (['a'..'z'] ++ ['A'..'Z']) $ [1..]

lookupPrio::Char-> Int
lookupPrio a = extract (Map.lookup a typePriority)
             where extract (Just a) = a
                   extract (Nothing) = error "nothing"
