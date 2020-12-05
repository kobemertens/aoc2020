module Day02
    ( solve
    , inRange
    ) where

data Entry = Entry { policy :: (Int, Int)
                   , char :: Char
                   , password :: String
                   } deriving (Show)


solve :: IO ()
solve = do
    lines <- getLines
    let parsed = parseLines lines
    putStr "Part1: "
    print $ length $ filter checkEntry parsed

    putStr "Part2: "
    print $ length $ filter checkEntry' parsed

checkEntry :: Entry -> Bool
checkEntry Entry {policy=p, char=c, password=pw} = uncurry (inRange (countElem c pw)) p

checkEntry' :: Entry -> Bool
checkEntry' Entry {policy=p, char=c, password=pw} = checkPos (fst p) c pw /= checkPos (snd p) c pw
  where
      checkPos :: (Eq a) => Int -> a -> [a] -> Bool
      checkPos i x xs = (xs !! (i-1)) == x

inRange :: (Ord a, Eq a) => a -> a -> a -> Bool
inRange e l h = l <= e && h >= e

countElem :: (Eq a) => a -> [a] -> Int
countElem c xs = length $ filter (== c) xs

parseLines :: [String] -> [Entry]
parseLines = map (parseLine . words)

parseLine :: [String] -> Entry
parseLine [p, c, pw] = Entry { policy=read p, char=head c, password=pw }

getLines :: IO [String]
getLines = lines <$> readFile "data/day02.txt"