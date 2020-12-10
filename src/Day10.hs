module Day10
    ( solve
    ) where

import Data.List

solve :: IO ()
solve = do
    l <- lines <$> readFile "data/day10.txt"
    let d = 0:map (read :: String -> Int) l
    putStr "Part1: "
    print $ part1 $ sort $ d
    putStr "Part2: "
    print $ part2 $ sort d

part1 :: [Int] -> Int
part1 xs =  multTuple . countDiffs $ zip xs (tail xs)

-- part2 :: [Int] -> Int
part2 l = foldl countConfs [(2, 2), (1, 1), (0, 1)] (drop 3 l)


-- take int and three last configurations
countConfs :: [(Int, Int)] -> Int -> [(Int, Int)]
countConfs hist c = (c, sum $ map (checkEl c) hist):init hist

checkEl :: Int -> (Int, Int) -> Int
checkEl cur (n, c)
    | cur - n <= 3 = c
    | otherwise = 0

multTuple (x, y) = x*(y+1)

countDiffs :: [(Int, Int)] -> (Int, Int)
countDiffs = foldl counter (0, 0)

counter (o, t) (f, s)
    | s - f == 1 = (o + 1, t)
    | s - f == 3 = (o, t + 1)
    | otherwise = (o, t)