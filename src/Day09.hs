module Day09
    (solve
    ) where

import Data.Maybe(isJust, fromJust)

solve :: IO ()
solve = do
    d <- lines <$> readFile "data/day09.txt"
    let parsed = map (read::String -> Int) d
    let n =  part1 (take 25 parsed) (drop 25 parsed)
    putStr "Part1: "
    print n
    putStr "Part2: "
    print $ part2 n parsed

part1 :: [Int] -> [Int] -> Int
part1 _ [] = error "All numbers are correct?!"
part1 p (x:xs)
    | findSum x p = part1 (tail p ++ [x]) xs
    | otherwise = x

part2 :: Int -> [Int] -> Int
part2 g l@(_:xs)
    | isJust set = minimum (fromJust set) + maximum (fromJust set)
    | otherwise = part2 g xs
  where
      set = findSet [] l g

findSum :: Int -> [Int] -> Bool
findSum n [] = False
findSum n (x:xs)
    | (n-x) `elem` xs = True
    | otherwise = findSum n xs

findSet :: [Int] -> [Int] -> Int -> Maybe [Int]
findSet _ [] _ = Nothing
findSet acc (x:todo) goal
    | x + sum acc > goal = Nothing
    | x + sum acc == goal = Just (x:acc)
    | otherwise = findSet (x:acc) todo goal