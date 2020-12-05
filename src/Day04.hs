module Day04
    ( solve
    ) where

import Data.List.Split


import Day02(inRange)

requiredFields = [ "byr"
                 , "iyr"
                 , "eyr"
                 , "hgt"
                 , "hcl"
                 , "ecl"
                 , "pid"
                 ]

solve :: IO ()
solve = do
    content <- readFile "data/day04.txt"
    putStr "Part1: "
    let a = map (splitWhen (`elem` " \n")) $ splitOn "\n\n" content
    print $ length $ filter (==True) $ map checkPassport a

    putStr "Part2: "
    print $ length $ filter (==True) $ map checkPassport' a

checkField :: String -> String -> Bool
checkField "byr" x = length x == 4 && inRange (read x :: Int) 1920 2002
checkField "iyr" x = length x == 4 && inRange (read x :: Int) 2010 2020
checkField "eyr" x = length x == 4 && inRange (read x :: Int) 2020 2030
checkField "hgt" x = checkHeight x
checkField "hcl" x = length x == 7 && checkHcl x
checkField "ecl" x = x `elem` eyeColors
  where
      eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
checkField "pid" x = length x == 9 && isInteger x
checkField "cid" _ = True
checkField _ _ = False

checkHcl :: String -> Bool
checkHcl (x:xs) = x == '#' && all (`elem` "0123456789abcdef") xs

checkHeight :: String -> Bool
checkHeight x
    | endswith "cm" x && isInteger (head $ splitOn "cm" x) && inRange (read $ head $ splitOn "cm" x) 150 193 = True
    | endswith "in" x && isInteger (head $ splitOn "in" x) && inRange (read $ head $ splitOn "in" x) 59 76 = True
    | otherwise = False

endswith :: String -> String -> Bool
endswith _ [] = False
endswith a (x:xs)
    | a == (x:xs) = True
    | length (x:xs) == length a = False
    | otherwise = endswith a xs

isInteger :: String -> Bool
isInteger = all (`elem` "0123456789")

checkPassport :: [String] -> Bool
checkPassport pp = containsAtLeast requiredFields $ getPassKeys pp
    where
        getPassKeys :: [String] -> [String]
        getPassKeys xs = map (head . splitOn ":") xs

checkPassport' :: [String] -> Bool
checkPassport' pp = containsAtLeast requiredFields (getPassKeys pp) && all checkEntry pp
    where
        getPassKeys :: [String] -> [String]
        getPassKeys xs = map (head . splitOn ":") xs
        checkEntry s = checkField (head (splitOn ":" s)) (last (splitOn ":" s))


containsAtLeast :: (Eq a) => [a] -> [a] -> Bool
containsAtLeast [] _ = True
containsAtLeast (n:ns) xs
    | n `elem` xs = containsAtLeast ns xs
    | otherwise = False