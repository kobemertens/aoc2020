module Day17
    ( solve
    , neighbourCoords
    ) where

import Data.Map (Map, fromList, empty, union, mapWithKey, findWithDefault, fold)

type Vector = (Int, Int, Int)
type PocketDimension = Map Vector Bool

solve :: IO ()
solve = do
    putStr "Part1: "
    f <- readFile "data/day17.txt"
    let initState = initStateFromStr f
    print $ countActive $ nState (initState `union` emptyState) 6
    -- print $ neighbourCoords (0, 0, 0)

dl :: Int
dl = 30

nState :: PocketDimension -> Int -> PocketDimension
nState pd 0 = pd
nState pd n = nState (nextState pd) (n-1)

nextState :: PocketDimension -> PocketDimension
nextState pd = mapWithKey (nextCellState pd) pd

nextCellState :: PocketDimension -> Vector -> Bool -> Bool
nextCellState pd v a
    | a = let x = countActiveNb pd v in x == 2 || x == 3
    | otherwise = let x = countActiveNb pd v in x == 3

countActiveNb :: PocketDimension -> Vector -> Int
countActiveNb pd v = length . filter id $ map mapFunc (neighbourCoords v)
  where
    mapFunc :: Vector -> Bool
    mapFunc v = findWithDefault False v pd

countActive :: PocketDimension -> Int
countActive = foldr f 0
  where
    f :: Bool -> Int -> Int
    f True acc = acc+1
    f _ acc = acc

emptyState :: PocketDimension
emptyState = fromList $ [((a, b, c), False) | a <- [-dl..dl], b <- [-dl..dl], c <- [-dl..dl]]

neighbourCoords :: Vector -> [Vector]
neighbourCoords v = map (addVector v) ds
  where
    ds = [(a, b, c) | a <- [-1..1], b <- [-1..1], c <- [-1..1], (a, b, c) /= (0, 0, 0)]

addVector :: Vector -> Vector -> Vector
addVector (x1, y1, z1) (x2, y2, z2) = (x1+x2, y1+y2, z1+z2)

initStateFromStr :: String -> PocketDimension
initStateFromStr s = foldl foldFunc empty $ zip [0..] (lines s)
  where
    foldFunc acc (rn, row) = acc `union` fromList [((rn, cn, 0), c=='#') | (cn, c) <- zip [0..] row]