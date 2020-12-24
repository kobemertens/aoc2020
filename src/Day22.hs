module Day22
    ( solve
    ) where

import NanoParsec
    ( number, reserved, runParser, spaces, split, Parser)
import Debug.Trace
import Data.Set (Set, empty, insert, member)

type Deck = [Int]
data Game = Game
    { decks :: (Deck, Deck)
    , history :: Set (Deck, Deck)
    } deriving (Show)

-- TODO create monadic game

solve :: IO ()
solve = do
    f <- readFile "data/day22.txt"
    let g = runParser deckParser2 f
    putStr "Part1: "
    print $ part1 g
    putStr "Part2: "
    print $ part2 g

part1 :: Game -> Int
part1 = countScore . fromEither . runGame gameStep

part2 :: Game -> Int
part2 = countScore . fromEither . runGame gameStep'

countScore :: Deck -> Int
countScore w = sum $ zipWith (*) [1..(length w)] (reverse w)

getWinner :: Game -> Maybe (Either Deck Deck)
getWinner (Game ([], w) _) = Just $ Right w
getWinner (Game (w, []) _) = Just $ Left w
getWinner (Game ds@(l, _) h)
    | ds `member` h = Just $ Left l
    | otherwise = Nothing

runGame :: (Game -> Game) -> Game -> Either Deck Deck
runGame f x = case getWinner x of
    (Just x) -> x
    Nothing -> (runGame f . f) x

fromEither :: Either a a -> a
fromEither (Left x) = x
fromEither (Right x) = x

gameStep :: Game -> Game
gameStep (Game ds@(x:xs, y:ys) h)
    | x > y     = Game (xs ++ [x, y], ys) (insert ds h)
    | otherwise = Game (xs, ys ++ [y, x]) (insert ds h)

gameStep' :: Game -> Game
gameStep' g@(Game ds@(x:xs, y:ys) h)
    | x > length xs || y > length ys = gameStep g
    | otherwise =
        case runGame gameStep' (Game (take x xs, take y ys) empty) of
            (Left _) -> Game (xs ++ [x, y], ys) (insert ds h)
            (Right _) -> Game (xs, ys ++ [y, x]) (insert ds h)

deckParser2 :: Parser Game
deckParser2 = do
    a <- deckParser
    spaces
    b <- deckParser
    return $ Game (a, b) empty

deckParser :: Parser Deck
deckParser = do
    reserved "Player"
    number
    reserved ":"
    spaces
    split "\n" number