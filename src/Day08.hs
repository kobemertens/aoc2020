module Day08
    ( solve
    ) where

import qualified Data.Set as Set
import Debug.Trace
import Data.List
import Data.Maybe

data Instruction
    = Instruction String Int
    deriving (Show)

data Interpreter = Interpreter Int Int

solve :: IO ()
solve = do
    lines <- lines <$> readFile "data/day08.txt"
    let program = zipWith parseLine (rangeFrom 0) lines
    print $ findFirstLoop (Interpreter 0 0) program Set.empty
    let programs = replaceOp "nop" "jmp" program ++ replaceOp "jmp" "nop" program
    let result = findLeft $ map (\program -> findFirstLoop (Interpreter 0 0) program Set.empty) programs
    print result

findLeft :: [Either a a] -> a
findLeft x = head $ mapMaybe leftToMaybe x
  where
    leftToMaybe (Left a) = Just a
    leftToMaybe (Right _) = Nothing

replaceOp :: String -> String -> [Instruction] -> [[Instruction]]
replaceOp old new program = map (replaceOpAtIndex new program) $ findIndices (\(Instruction instr _) -> instr == old) program

replaceOpAtIndex :: String ->  [Instruction] -> Int -> [Instruction]
replaceOpAtIndex new program i = take i program ++ (newInstruction operand : drop (i+1) program)
  where
      (Instruction _ operand) = program!!i
      newInstruction op = Instruction new op

findFirstLoop :: Interpreter -> [Instruction] -> Set.Set Int -> Either Int Int
findFirstLoop interpreter program seen
    | pc >= length program = Left acc
    | pc `Set.member` seen = Right acc
    | otherwise = findFirstLoop (Interpreter pc acc) program (Set.insert pc seen)
  where
      (Interpreter pc acc) = runInstruction interpreter program

runInstruction :: Interpreter -> [Instruction] -> Interpreter
runInstruction (Interpreter pc acc) program
    = Interpreter (newPc pc instruction) (newAcc acc instruction)
  where
      instruction = program!!pc
      newPc pc (Instruction "jmp" op) = pc + op
      newPc pc _ = pc + 1
      newAcc acc (Instruction "acc" op) = acc + op
      newAcc acc _ = acc

parseLine :: Int -> String -> Instruction
parseLine i s = Instruction inst (parseInt op)
  where
    [inst, op] = words s

parseInt :: String -> Int
parseInt ('+':xs) = read xs
parseInt x = read x

rangeFrom :: Int -> [Int]
rangeFrom n = n:rangeFrom (n + 1)
