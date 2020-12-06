module Day06
    ( solve
    ) where

import Data.Set (Set
                , fromList
                , intersection)
import Data.List.Split ( splitOn )


solve :: IO ()
solve = do
    lines <- getLines
    let questions = map (splitOn "\n") $ splitOn "\n\n" lines
    putStr "Part1: "
    print $ sum $ map getUniqueCount questions

    putStr "Part2: "
    print $ sum $ map getIntersectionCount questions


getUniqueCount :: [String] -> Int
getUniqueCount ss = length $ (fromList :: String -> Set Char) $ concat ss

getIntersectionCount :: [String] -> Int
getIntersectionCount ss = length $ foldl intersection (fromList "abcdefghijklmnopqrstuvwxyz") $ map fromList ss


getLines :: IO String
getLines = readFile "data/day06.txt"