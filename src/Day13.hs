module Day13
    (solve
    ) where

import Data.List.Split(splitOn)
import Math.NumberTheory.Euclidean ( extendedGCD )

solve :: IO ()
solve = do
    l <- lines <$> readFile "data/day13.txt"
    let f = getFiltered $ last l
    -- let f = [(3, 2), (4, 3), (5, 2)]
    let eList = getElist f
    let total = sum $ map tupleProd $  zip eList $ map snd f
    let anothertotal = product $ map fst f
    let newtotal = total `mod` anothertotal
    print newtotal
    -- print $ modularInv 3 5
    return ()

tupleProd :: (Integer, Integer) -> Integer
tupleProd (x, y) = x*y

getElist :: [(Integer, Integer)] -> [Integer]
getElist m = map (getE total) m
  where
      total = product $ map fst m

getE :: Integer -> (Integer, Integer) -> Integer
getE total (n, _) = big_n * (third $ extendedGCD (toInteger n) (toInteger big_n))
  where
      big_n = (total `div` n)

third :: (a, a, a) -> a
third (_, _, x) = x

getFiltered :: String -> [(Integer, Integer)]
getFiltered s = map (\(x, y) -> (read x :: Integer, y)) $ filter (\(i, j) -> i /= "x") $ zip (splitOn "," s) (range 0)

range :: Integer -> [Integer]
range n = (-n):range (n+1)