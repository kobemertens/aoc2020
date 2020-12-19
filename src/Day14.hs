module Day14
    ( solve
    ) where

import Data.Map(Map, insert, empty, fold)
import NanoParsec
    ( Parser, runParser, reserved, spaces, number, parens, plus, word )

type Mask = String
type Memory = Map Int Int
type Program = [Block]
type Instruction = (Int, Int)
--              (address, bitstring)
data Block = Block Mask [Instruction] deriving ( Show )

solve :: IO ()
solve = do
    l <- readFile "data/day14.txt"
    let program = runParser programParser l
    putStr "Part1: "
    let mem = runProgram empty program
    print $ sum mem
    let mem = runProgram' empty program
    putStr "Part2: "
    print $ sum mem
    return ()

runProgram :: Memory -> Program -> Memory
runProgram = foldl runBlock

runProgram' :: Memory -> Program -> Memory
runProgram' = foldl runBlock'

runBlock :: Memory -> Block -> Memory
runBlock mem (Block m is) = foldl (runInstruction m) mem is

runBlock' :: Memory -> Block -> Memory
runBlock' mem (Block m is) = foldl (runInstruction' m) mem is

runInstruction :: Mask -> Memory -> Instruction -> Memory
runInstruction m mem (address, value) = insert address nvalue mem
  where
    nvalue = (binToDec . applyMask m . decToBinPadded) value

runInstruction' :: Mask -> Memory -> Instruction -> Memory
runInstruction' m mem (a, v) = insertMultiple (map binToDec as) v mem
  where
    binaryAddress = decToBinPadded a
    as = applyMask' m binaryAddress

insertMultiple :: [Int] -> Int -> Memory -> Memory
insertMultiple [] _ m = m
insertMultiple (k:ks) v m = insertMultiple ks v (insert k v m)

applyMask :: String -> String -> String
applyMask m s = zipWith (curry maskf) m s
  where
    maskf (m, s)
        | m == 'X' = s
        | otherwise = m

applyMask' :: String -> String -> [String]
applyMask' m s = applyMask'' m s [""]
  where
    applyMask'' :: String -> String -> [String] -> [String]
    applyMask'' [] [] acc = acc
    applyMask'' (m:ms) (s:ss) acc = case m of
        '0' -> applyMask'' ms ss [p++[s] | p <- acc]
        '1' -> applyMask'' ms ss [p++"1" | p <- acc]
        'X' -> applyMask'' ms ss ([p++"1" | p <- acc] ++ [p++"0" | p <- acc])

binToDec :: String -> Int
binToDec = foldl processBit 0
  where
    processBit :: Int -> Char -> Int
    processBit x u
        | '1' == u = x*2 + 1
        | otherwise = x*2

decToBinPadded :: Int -> String
decToBinPadded = pad . decToBin
  where
    pad s = replicate (36 - length s) '0' ++ s

decToBin :: Int -> String
decToBin i = decToBin' i ""
  where
    decToBin' :: Int -> String -> String
    decToBin' 0 m = '0':m
    decToBin' 1 m = '1':m
    decToBin' x m = case x `mod` 2 of
        0 -> decToBin' (x `div` 2) ('0':m)
        1 -> decToBin' ((x-1) `div` 2) ('1':m)

programParser :: Parser Program
programParser = do
    a <- plus blockParser
    spaces
    return a

blockParser :: Parser Block
blockParser = do
    reserved "mask = "
    a <- word
    spaces
    is <- plus instructionParser
    return $ Block a is

instructionParser :: Parser Instruction
instructionParser = do
    reserved "mem"
    a <- parens "[" "]" number
    spaces
    reserved "="
    spaces
    b <- number
    spaces
    return (a, b)
