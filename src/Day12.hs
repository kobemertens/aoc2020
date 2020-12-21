module Day12
    ( solve
    ) where

data Instruction
    = North Int
    | East Int
    | South Int
    | West Int
    | Forward Int
    | RotLeft Int
    | RotRight Int
    deriving ( Show )

-- Ferry xpos ypos xwayp ywayp
data Ferry = Ferry Int Int Int Int
    deriving ( Show )


solve :: IO ()
solve = do
    l <- lines <$> readFile "data/day12.txt"
    let instr = map parseLine l
    putStr "Part1: "
    print $ part1 (Ferry 0 0 1 0) instr

    putStr "Part2: "
    print $ part2 (Ferry 0 0 10 1) instr

part1 :: Ferry -> [Instruction] -> Int
part1 f is = let (Ferry x y _ _) = driveFerry f is in abs x + abs y

part2 :: Ferry -> [Instruction] -> Int
part2 f is = let (Ferry x y _ _) = driveFerry' f is in abs x + abs y

driveFerry :: Ferry -> [Instruction] -> Ferry
driveFerry = foldl ferryStep

driveFerry' :: Ferry -> [Instruction] -> Ferry
driveFerry' = foldl ferryStep'

ferryStep :: Ferry -> Instruction -> Ferry
ferryStep (Ferry x y wx wy) (North s) = Ferry x (y+s) wx wy
ferryStep (Ferry x y wx wy) (East s) = Ferry (x+s) y wx wy
ferryStep (Ferry x y wx wy) (South s) = Ferry x (y-s) wx wy
ferryStep (Ferry x y wx wy) (West s) = Ferry (x-s) y wx wy
ferryStep (Ferry x y wx wy) (Forward s) = Ferry (x+s*wx) (y+s*wy) wx wy
ferryStep f (RotRight s) = rotateRight f s
ferryStep f (RotLeft s) = rotateRight f (360 - s)

ferryStep' :: Ferry -> Instruction -> Ferry
ferryStep' (Ferry x y wx wy) (North s) = Ferry x y wx (wy+s)
ferryStep' (Ferry x y wx wy) (East s) = Ferry x y (wx+s) wy
ferryStep' (Ferry x y wx wy) (South s) = Ferry x y wx (wy-s)
ferryStep' (Ferry x y wx wy) (West s) = Ferry x y (wx-s) wy
ferryStep' (Ferry x y wx wy) (Forward s) = Ferry (x+s*wx) (y+s*wy) wx wy
ferryStep' f (RotRight s) = rotateRight f s
ferryStep' f (RotLeft s) = rotateRight f (360 - s)

rotateRight :: Ferry -> Int -> Ferry
rotateRight f 0 = f
rotateRight (Ferry x y px py) d = let (nx, ny) = rotateAroundOrigin d (px, py) in Ferry x y nx ny

rotateAroundOrigin :: Int -> (Int, Int) -> (Int, Int)
rotateAroundOrigin 0 c = c
rotateAroundOrigin 90 (x, y) = (y, -x)
rotateAroundOrigin 180 (x, y) = (-x, -y)
rotateAroundOrigin 270 (x, y) = (-y, x)

parseLine :: String -> Instruction
parseLine (x:xs)
    | x == 'F' = Forward (read xs)
    | x == 'R' = RotRight (read xs)
    | x == 'L' = RotLeft (read xs)
    | x == 'N' = North (read xs)
    | x == 'E' = East (read xs)
    | x == 'S' = South (read xs)
    | x == 'W' = West (read xs)
    | otherwise = error "Unknown input"