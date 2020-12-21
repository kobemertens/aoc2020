module Day21
    ( solve
    ) where

import Prelude hiding (null)
import NanoParsec
    ( parens, plus, reserved, runParser, spaces, split, Parser, alpha )
import Data.Set ( Set, fromList, intersection, null, union, empty, member, toList, difference)
import Data.List.Key (sort)

type Ingredient = String
type Allergen = String
type FoodList = [FoodItem]

type FoodItem = (Set Ingredient, Set Allergen)

solve :: IO ()
solve = do
    f <- readFile "data/day21.txt"
    let fl = runParser foodListParser f
    putStr "Part1: "
    print $ part1 fl
    putStr "Part2: "
    print $ part2 fl

-- part1 :: FoodList -> Int
part1 fl = count (combine $ something fl) fl

part2 fl = join "," $ map snd $ sort fst $ filterCombined $ something fl

join :: [a] -> [[a]] -> [a]
join _ [] = []
join _ [x] = x
join s (x:xs) = x ++ s ++ join s xs

count :: Set Ingredient -> FoodList -> Int
count _ [] = 0
count is (f:fs) = length (difference (fst f) is) + count is fs

filterCombined :: [(Allergen, Set Ingredient)] -> [(Allergen, Ingredient)]
filterCombined fs
    | all (\(_, a) -> length a == 1) fs = map (\(a, b) -> (a, head (toList b))) fs
    | otherwise = let found = foldl union empty $ map snd $ filter (\(_, s) -> length s == 1) fs
                      newFilter = map (filterFunc found) fs
                  in filterCombined newFilter
  where
    filterFunc :: Set Ingredient -> (Allergen, Set Ingredient) -> (Allergen, Set Ingredient)
    filterFunc f (a, is)
        | length is == 1 = (a, is)
        | otherwise = (a, difference is f)

-- combine :: [(Allergen, Set Ingredient)] -> FoodItem
combine l = foldl union empty $ map snd l

something :: FoodList -> [(Allergen, Set Ingredient)]
something fs = map (another fs) (toList (distinctAller fs))

distinctAller :: FoodList -> Set Allergen
distinctAller fs = foldl union empty (map snd fs)

another :: FoodList -> Allergen -> (Allergen, Set Ingredient)
another fl a = (a, foldl1 intersection (map fst (filter (\(_, as) -> a `member` as) fl)))

foodListParser :: Parser FoodList
foodListParser = plus foodItemParser

foodItemParser :: Parser FoodItem
foodItemParser = do
    ingr <- plus (alpha <* spaces)
    al <- parens "(" ")" (reserved "contains" *> split ", " alpha)
    spaces
    return (fromList ingr, fromList al)