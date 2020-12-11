module Day11
    ( solve
    ) where

import Day02 (inRange)

type Point = (Int, Int)
data Seat = Occupied | Floor | Empty deriving (Eq, Show)
type Grid = Point -> Seat

gameOfSeats :: Grid -> Int -> Grid
gameOfSeats init 0 p
    = init p
gameOfSeats init n p
    = nextStep (gameOfSeats init (n - 1) p)
               (map (gameOfSeats init (n - 1)) (adjacents p))
  where
    nextStep :: Seat -> [Seat] -> Seat
    nextStep Occupied adj
        | count Occupied adj >= 4 = Empty
        | otherwise = Occupied
    nextStep Empty adj
        | count Occupied adj == 0 = Occupied
        | otherwise = Empty
    nextStep Floor _ = Floor


count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

adjacents :: Point -> [Point]
adjacents (x,y)
    = [(x+m, y+n) | m <- [-1,0,1], n <- [-1,0,1], (m,n) /= (0,0)]

getGrid :: [String] -> (Int, Int) -> Seat
getGrid l (i, j)
    | inRange i 0 ((length l) - 1) && inRange j 0 ((length (head l)) - 1) = parseSeat $ (l!!i)!!j
    | otherwise = Floor
  where
      parseSeat c
          | c == 'L' = Empty
          | c == '#' = Occupied
          | c == '.' = Floor

solve :: IO ()
solve = do
    lines <- lines <$> readFile "data/day11.txt"
    let grid = getGrid lines
    print $ gameOfSeats grid 100 (0, 3)
    return ()