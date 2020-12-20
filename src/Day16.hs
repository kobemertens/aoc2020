module Day16
    ( solve
    ) where

import NanoParsec
    ( number, plus, reserved, runParser, spaces, split, word, Parser )
import Day02(inRange)
import Data.List(isPrefixOf)

data Field = Field String [Range] deriving (Show, Eq)
type Header = [Field]
type Range = (Int, Int)
type Ticket = [Int]

solve :: IO ()
solve = do
    d <- readFile "data/day16.txt"
    let (header, ticket, nt) = runParser fileParser d
    putStr "Part1: "
    print $ part1 header nt
    putStr "Part2: "
    print $ part2 header nt ticket


part1 :: Header -> [Ticket] -> Int
part1 h nt = let rs = getRanges h
                 allTickets = concat nt
             in sum $ filter (\t -> (not . any (uncurry (inRange t))) rs) allTickets

part2 :: Header -> [Ticket] -> Ticket -> Int
part2 h ts t = let filteredTickets = filterTickets (getRanges h) ts
                   fieldOrder = getFieldNames $ cleanSieve $ getFieldOrder h filteredTickets
               in getAnswer $ zip fieldOrder t
  where
      getAnswer :: [(String, Int)] -> Int
      getAnswer fs = product $ map snd $ filter (\(s,_) -> "departure" `isPrefixOf` s) fs

cleanSieve :: (Eq a) => [[a]] -> [a]
cleanSieve l
    | all (\s -> length s == 1) l = map head l
    | otherwise = let found = map head $ filter (\s -> length s == 1) l
                      newSieve = map (filterFunc found) l
                  in cleanSieve newSieve
  where
    filterFunc :: (Eq a) => [a] -> [a] -> [a]
    filterFunc f item
        | length item == 1 = item
        | otherwise = filter (`notElem` f) item

getFieldOrder :: Foldable t => [Field] -> t Ticket -> [[Field]]
getFieldOrder h = foldl foldFunc (replicate (length h) h)
  where
    foldFunc :: [[Field]] -> Ticket -> [[Field]]
    foldFunc fs t = zipWith sieveFields fs t

-- Possible fields -> value -> new possible fields
sieveFields :: [Field] -> Int -> [Field]
sieveFields fs i = filter (\(Field _ rs) -> any (uncurry (inRange i)) rs) fs

getRanges :: Header -> [Range]
getRanges = concatMap (\(Field _ rs) -> rs)

getFieldNames :: Header -> [String]
getFieldNames = map (\(Field s _) -> s)

filterTickets :: [Range] -> [Ticket] -> [Ticket]
filterTickets _ [] = []
filterTickets rs (t:ts)
    | all (\x -> any (uncurry (inRange x)) rs) t = t:filterTickets rs ts
    | otherwise = filterTickets rs ts

fileParser :: Parser (Header, Ticket, [Ticket])
fileParser = do
    header <- headerParser
    ticket <- ticketParser
    spaces
    nt <- ntParser
    return (header, ticket, nt)

ticketParser :: Parser Ticket
ticketParser = do
    reserved "your ticket:"
    split "," number

ntParser :: Parser [Ticket]
ntParser = do
    reserved "nearby tickets:"
    spaces
    plus $ spaces >> split "," number

headerParser :: Parser Header
headerParser = do
    plus fieldParser

rangeParser :: Parser Range
rangeParser = do
    r <- split "-" number
    let [a, b] = r
    return (a, b)

fieldParser :: Parser Field
fieldParser = do
    k <- word
    spaces
    rs <- split " or " rangeParser
    spaces
    return $ Field k rs