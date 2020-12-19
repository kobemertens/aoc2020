module Day16
    ( solve
    ) where

import NanoParsec
import Debug.Trace
import Day02(inRange)

data Field = Field String [Range] deriving (Show)
type Header = [Field]
type Range = (Int, Int)
type Ticket = [Int]

solve :: IO ()
solve = do
    d <- readFile "data/day16.txt"
    let (header, ticket, nt) = runParser fileParser d
    putStr "Part1: "
    print $ part1 header nt
    -- putStr "Part2: "
    -- print $ part2

part1 :: Header -> [Ticket] -> Int
part1 h nt = let rs = getRanges h
                 allTickets = concat nt
             in sum $ filter (\t -> (not . any (uncurry (inRange t))) rs) allTickets

getRanges :: Header -> [Range]
getRanges = concatMap (\(Field _ rs) -> rs)

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