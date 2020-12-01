module Day01
    ( solve
    ) where

solve :: IO ()
solve = do
    a <- parseInput
    let parsed = map read a :: [Int]

    putStr "Part 1: "
    print $ findN $ subsets 2 parsed

    putStr "Part 2: "
    print $ findN $ subsets 3 parsed

findN :: (Num a, Eq a) => [[a]] -> Maybe a
findN xs = maybeMult $ head $ filter sumEquals2020 xs
  where
      sumEquals2020 xs = foldl (+) 0 xs == 2020
      maybeMult [] = Nothing
      maybeMult xs = Just $ foldl (*) 1 xs

subsets :: Int -> [a] -> [[a]]
subsets 0 _ = [[]]
subsets _ [] = []
subsets n (x : xs) = map (x :) (subsets (n - 1) xs) ++ subsets n xs


parseInput = lines <$> readFile "data/day01.txt"