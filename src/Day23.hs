 module Day23
    ( solve
    ) where

import Prelude hiding (showChar)
import Data.List ( elemIndex )
import Data.Map (Map, (!), insert, empty, fromList)
import Debug.Trace

type Label = Int
type Cups = Map Label Label
type GameState = (Label, Cups)

solve :: IO ()
solve = do
    c  <- map readChar <$> readFile "data/day23.txt"
    -- print c
    let g = buildGameState c
    let (fl, fcs) = nSteps g 100
    print $ map showChar $ getSeqFromCups fcs (fcs!1) 8

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
takeStep (l, cs) = let s@[_, _, c] = getSeqFromCups cs (cs!l) 3
                       ip = findInsertLabel cs l s
                       cs' = insertAt cs ip s
                       cs'' = insert l (cs!c) cs'
                   in (cs''!l, cs'')

findInsertLabel :: Cups -> Label -> [Label] -> Label
findInsertLabel cs l ls
    | (l - 1) == 0      = findInsertLabel cs (mxl+1) ls
    | (l - 1) `elem` ls = findInsertLabel cs (l-1) ls
    | otherwise         = l - 1
  where mxl = 9

-- |Inserts a given sequence at a given point
insertAt :: Cups -> Label -> [Label] -> Cups
insertAt cs l [a, _, c] = insert l a (insert c ln cs)
  where
    ln = cs!l

-- |Retrieves a sequence of n labels starting from the given label
getSeqFromCups :: Cups -> Label -> Int -> [Label]
getSeqFromCups _ _ 0 = []
getSeqFromCups cs l n = l:getSeqFromCups cs (cs!l) (n-1)

readChar :: Char -> Int
readChar c = read [c]

showChar :: Int -> Char
showChar = head . show