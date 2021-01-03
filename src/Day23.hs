 module Day23
    ( solve
    ) where

import Prelude hiding (showChar)
import Data.Char (digitToInt, intToDigit)
import Data.Map (Map, (!), insert, empty, fromList)

type Label = Int
-- | Maps a label to its right neighbour
type Cups = Map Label Label
type GameState = (Label, Cups)

solve :: IO ()
solve = do
    c  <- map digitToInt <$> readFile "data/day23.txt"
    putStr "Part1: "
    print $ part1 c
    putStr "Part2: "
    print $ part2 c

part1 :: [Int] -> String
part1 c = let g = buildGameState c
              (fl, fcs) = nSteps g 100
          in map intToDigit $ getSeqFromCups fcs (fcs!1) 8

part2 :: [Int] -> String
part2 c = let g = buildGameState $ c ++ [10..1000000]
              (fl, fcs) = nSteps g 10000000
        in show $ product $ getSeqFromCups fcs (fcs!1) 2

-- |Progress the game n steps
nSteps :: GameState -> Int -> GameState
nSteps g n = iterate takeStep g !! n

-- |Builds a game state from the puzzle input
buildGameState :: [Label] -> GameState
buildGameState ls =
    (head ls
    , fromList $ zip ls (tail ls ++ [head ls]))

-- |Progress the game one step
takeStep :: GameState -> GameState
takeStep (l, cs) = (cs''!l, cs'')
  where
    s@[_, _, c] = getSeqFromCups cs (cs!l) 3
    ip          = findInsertLabel cs l s
    cs'         = insertAt cs ip s
    cs''        = insert l (cs!c) cs'

-- |Finds the next label to insert the subList
findInsertLabel :: Cups -> Label -> [Label] -> Label
findInsertLabel cs l ls
    | (l - 1) == 0      = findInsertLabel cs (mxl+1) ls
    | (l - 1) `elem` ls = findInsertLabel cs (l-1) ls
    | otherwise         = l - 1
  where
    mxl = length cs

-- |Inserts a given sequence at a given point
insertAt :: Cups -> Label -> [Label] -> Cups
insertAt cs l [a, _, c] = let ln = cs!l
                          in insert l a (insert c ln cs)

-- |Retrieves a sequence of n labels starting from the given label
getSeqFromCups :: Cups -> Label -> Int -> [Label]
getSeqFromCups _ _ 0 = []
getSeqFromCups cs l n = l:getSeqFromCups cs (cs!l) (n-1)