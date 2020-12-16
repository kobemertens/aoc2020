-- http://dev.stephendiehl.com/fun/002_parsers.html

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> Maybe (a,String) }

runParser :: Parser a -> String -> a
runParser m s =
    case parse m s of
        Just (res, []) -> res
        Just (_, rs)   -> error $ "Parser did not consume entire stream." ++ rs
        Nothing           -> error "Parser error."

item :: Parser Char
item = Parser $ \s ->
    case s of
        [] -> Nothing
        (c:cs) -> Just (c,cs)

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s ->
    case parse p s of
        (Just (res, rem)) -> parse (f res) rem
        Nothing -> Nothing

unit :: a -> Parser a
unit a = Parser $ \s -> Just (a, s)

instance Functor Parser where
    -- return new parser that applies the function
    -- to every item in the original parser
    fmap f (Parser cs) = Parser $ \s ->
        case cs s of
            (Just (a, b)) -> Just (f a, b)
            Nothing -> Nothing

instance Applicative Parser where
    pure = return
    (Parser cs1) <*> (Parser cs2) = Parser $ \s ->
        case cs1 s of
            Nothing -> Nothing
            (Just (f, s1)) ->
                case cs2 s1 of
                    (Just (a, s2)) -> Just (f a, s2)
                    Nothing -> Nothing


instance Monad Parser where
    return = unit
    (>>=)  = bind

instance Alternative Parser where
    empty = Parser (const Nothing)
    (<|>) = option

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of
        Nothing -> parse q s
        res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
    if p c
    then return c
    else empty

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = return []
string (c:cs) = do
    char c
    string cs
    return (c:cs)

token :: Parser a -> Parser a
token p = do
    a <- p
    spaces
    return a

reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf "\n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
    s <- string "-" <|> return []
    cs <- some digit
    return $ read (s ++ cs)

-- left right
parens :: String -> String -> Parser a -> Parser a
parens l r m = do
    reserved l
    n <- m
    reserved r
    return n

-- match zero or more occurrences
star :: Parser a -> Parser [a]
star p = plus p <|> pure []

-- match one or more occurrences
plus :: Parser a -> Parser [a]
plus p = (:) <$> p <*> star p

word :: Parser String
word = plus $ satisfy (`notElem` "\n\r")