module Main where
import Data.Char (isNumber)
import Debug.Trace
import Text.Printf

main :: IO ()
main = do
  file <- readFile "input1b.txt"
  let stacks = parseStacks file
  let instructions = parseInstructions file

  -- Ex1
  let sim1 = foldl (\tempStack instr -> execute instr tempStack reverse) stacks instructions
  print . evaluate $ sim1
  -- Ex2
  let sim2 = foldl (\tempStack instr -> execute instr tempStack (\s->s)) stacks instructions
  print . evaluate $ sim2

evaluate::[String]->String
evaluate stacks =
          let headOrEmpty str = if 0 == length str then "_" else [head str]
              heads = map (\e -> headOrEmpty e) stacks
          in  foldr (\str acc -> str ++ acc) "" heads

execute::(Int,Int,Int)->[String]->(String->String)->[String]
execute instruction stacks pickupMode =
          let (nrToMove, from, to) = instruction
              ogFromStack = stacks !! (from-1)
              ogToStack = stacks !! (to-1)

              cratesToMove = pickupMode $ take nrToMove ogFromStack

              newFromStack = drop nrToMove ogFromStack
              newToStack = cratesToMove ++ ogToStack

              fromInsertedStack = insert stacks newFromStack (from-1)
          in  insert fromInsertedStack newToStack (to-1)

insert::[String]->String->Int->[String]
insert stacks newStack index =
          let split = splitAt (index) stacks
          in  (fst split) ++ [newStack] ++ (drop 1 $ snd split)

parseInstructions::String->[(Int, Int, Int)]
parseInstructions input =
          let inputLines = lines input
              cutoff = determineCutoff inputLines
              instructionLinesRaw = drop (cutoff + 1) inputLines
              parseInstructionLine line =
                            let separated = words line
                            in  (read (separated !! 1), read(separated !! 3), read(separated !! 5))
          in map parseInstructionLine instructionLinesRaw

parseStacks::String->[String]
parseStacks input =
          let inputLines = lines input
              cutoff = determineCutoff inputLines
              stackCount = nrOfStacks inputLines (cutoff - 1)
              parsedStackLines = reverse . map parseStackLine $ take (cutoff - 1) $ inputLines
          in  map (\stackIndex -> createStack parsedStackLines stackIndex) [1..stackCount]

createStack::[[(Int, Char)]]->Int->String
createStack stackLines stackIndex = foldl (\acc a -> (select a stackIndex) ++ acc) "" stackLines

select::[(Int, Char)]->Int->String
select [] _ = ""
select input index =
          let filtered = filter (\(i,_) -> i == index) input
          in if 0 == length filtered then "" else (snd . head $ filtered):[]

nrOfStacks::[String]->Int->Int
nrOfStacks lines i =
          let stackLine = lines !! i
          in  length [1,5..length stackLine]

determineCutoff::[String]->Int
determineCutoff lines = succ . fst . head . filter (\(_, _:a:xs)-> isNumber a) $ zip [0..] lines

parseStackLine::String->[(Int,Char)]
parseStackLine line =
          let indices = [1,5..length line]
              values = map (\index -> line !! index) indices
              paired = zip [1..] values
          in  filter (\(_, v) -> elem v ['A'..'Z']) paired
