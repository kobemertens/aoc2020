module Day05
    ( solve
    ) where

import Data.List ( find, sort )

solve :: IO ()
solve = do
    lines <- getLines
    let seatIds = getSeatIds lines
    let sortedIds = sort seatIds
    putStr "Part1: "
    print $ head sortedIds
    putStr "Part2: "
    print $ find (\(x, y) -> x /= y - 1) $ zip sortedIds (tail sortedIds)

getSeatIds :: [String] -> [Int]
getSeatIds = map getSeatId
  where
      getSeatId :: String -> Int
      getSeatId s = getRow s * 8 + getColumn s
      getRow s = foldl (parseSeatChar 'B') 0 (take 7 s)
      getColumn s = foldl (parseSeatChar 'R') 0 (drop 7 s)

parseSeatChar :: Char -> Int -> Char -> Int
parseSeatChar u x c
    | c == u = x*2+1
    | otherwise = x*2

getLines :: IO [String]
getLines = lines <$> readFile "data/day05.txt"
