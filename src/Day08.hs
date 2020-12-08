module Day08
    ( solve
    ) where

import Data.Set(Set, insert, member, empty)
import Data.Either ( fromLeft, fromRight )

data Instruction
    = Jmp Int
    | Acc Int
    | Nop Int
    deriving (Show)

data State = State Int Int

type Program = [Instruction]

solve :: IO ()
solve = do
    lines <- lines <$> readFile "data/day08.txt"
    let program = map parseLine lines
    putStr "Part 1: "
    print $ part1 program
    putStr "Part 2: "
    print $ part2 [] program

swapInstruction :: Instruction -> Instruction
swapInstruction (Jmp a) = Nop a
swapInstruction (Nop a) = Jmp a
swapInstruction x = x

part1 :: Program -> Int
part1 =  fromRight 0 . runProgram

part2 :: Program -> Program -> Int
part2 b (x:xs)
    | hasFinished result = fromLeft 0 result
    | otherwise = part2 (b ++ [x]) xs
  where
      result = runProgram (swapInstruction x:xs)

hasFinished :: Either Int Int -> Bool
hasFinished (Left _) = True
hasFinished (Right _) = False

runProgram :: Program -> Either Int Int
runProgram program = runProgram' (State 0 0) program empty

runProgram' :: State -> Program -> Set Int -> Either Int Int
runProgram' s p seen
    | pc >= length p = Left acc
    | pc `member` seen = Right acc
    | otherwise = runProgram' ns p (insert pc seen)
  where
      ns@(State pc acc) = programStep p s

runInstruction :: Instruction -> State -> State
runInstruction (Jmp a) (State pc acc) = State (pc+a) acc
runInstruction (Acc a) (State pc acc) = State (pc+1) (acc+a)
runInstruction _ (State pc acc) = State (pc+1) acc

programStep :: Program -> State -> State
programStep program s@(State pc _) = let instruction = program!!pc in runInstruction instruction s

parseLine :: String -> Instruction
parseLine s
    | inst == "acc" = Acc $ parseInt op
    | inst == "jmp" = Jmp $ parseInt op
    | inst == "nop" = Nop $ parseInt op
  where
    [inst, op] = words s

parseInt :: String -> Int
parseInt ('+':xs) = read xs
parseInt x = read x