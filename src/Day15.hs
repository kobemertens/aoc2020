module Day15
    ( solve
    ) where

import Prelude hiding (lookup)
import NanoParsec ( number, split, Parser, runParser )
import Data.List(elemIndex)
import Data.Map(Map, insert, empty, (!), fromList, lookup)

data GameState = GameState
    { len :: Int
    , current :: Int
    , posMap :: Map Int Int
    } deriving (Show)

solve :: IO ()
solve = do
    f <- readFile "data/day15.txt"
    let xs = runParser listParser f
    putStr "Part1: "
    print $ current $ stepNTimes (2020 - length xs) (initState xs)
    putStr "Part2: "
    print $ current $ stepNTimes (30000000 - length xs) (initState xs)

stepNTimes :: Int -> GameState -> GameState
stepNTimes 0 g = g
stepNTimes n g = stepNTimes (n-1) (nextState g)

appendHistory :: Int -> GameState -> [Int]
appendHistory 0 g = [current g]
appendHistory n g = current g:appendHistory (n-1) (nextState g)

initState :: [Int] -> GameState
initState xs = let initMap = fromList $ zip (init xs) (range 0)
               in GameState (length xs - 1) (last xs) initMap

nextState :: GameState -> GameState
nextState (GameState le c pm) =
    case lookup c pm of
        (Just x) -> GameState (le+1) (le - x) (insert c le pm)
        Nothing -> GameState (le+1) 0 (insert c le pm)

range :: Int -> [Int]
range n = n:range (n+1)

listParser :: Parser [Int]
listParser = split "," number