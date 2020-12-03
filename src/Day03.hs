module Day03
    ( solve
    ) where

solve :: IO ()
solve = do
    lines <- getLines
    putStr "Part1: "
    print $ countTrees 3 1 lines

    putStr "Part2: "
    let dirs = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
    print $ product $ [countTrees r d lines | (r, d) <- dirs]

countTrees :: Int -> Int -> [String] -> Int
countTrees r d xs = snd $ foldl checkString (r, 0) $ loopDropN (d-1) $ drop d xs
  where
      checkString :: (Int, Int) -> String -> (Int, Int)
      checkString (i, c) s
          | (cycle s !! i) == '#' = (i + r, c + 1)
          | otherwise = (i + r, c)

loopDropN :: Int -> [a] -> [a]
loopDropN _ [] = []
loopDropN 0 xs = xs
loopDropN n (x:xs) = x:loopDropN n (drop n xs)

getLines :: IO [String]
getLines = lines <$> readFile "data/day03.txt"