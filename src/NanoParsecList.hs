-- http://dev.stephendiehl.com/fun/002_parsers.html

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsecList where

import Data.Char ( isDigit, isAlpha )
import Control.Monad ( MonadPlus(..) )
import Control.Applicative ( Alternative(..) )

newtype Parser a = Parser { parse :: String -> [(a,String)] }

runParser :: Parser a -> String -> a
runParser m s = let l = parse m s in hasParseResult l
  where
    hasParseResult [] = error "Parser error."
    hasParseResult ((a, ""):ls) = a
    hasParseResult (l:ls) = hasParseResult ls

succeeds :: Parser a -> String -> Bool
succeeds m s = let l = parse m s in hasParseResult l
  where
    hasParseResult [] = False
    hasParseResult ((a, ""):ls) = True
    hasParseResult (l:ls) = hasParseResult ls

item :: Parser Char
item = Parser $ \s ->
    case s of
        "" -> []
        (c:cs) -> [(c,cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s -> concatMap (\(a, s') -> parse (f a) s') $ parse p s

unit :: a -> Parser a
unit a = Parser $ \s -> [(a, s)]

instance Functor Parser where
    -- return new parser that applies the function
    -- to the item in the original parser
    fmap f p = Parser $ \s -> [(f a, s') | (a, s') <- parse p s]

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s -> [(f a, s2) | (f, s1) <- cs1 s, (a, s2) <- cs2 s1])

instance Monad Parser where
    return = unit
    -- combines parser functions in a way that the
    -- remaining string gets passed automatically
    (>>=)  = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
    case parse p s of
        [] -> parse q s
        res -> res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c ->
    if p c
    then return c
    else empty

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (`elem` s)

-- | Parses one or more occurrences of `p` separated by
-- op and returns a value obtained by recursing until
-- failure on the left hand side of the stream
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

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

-- also removes spaces after word
reserved :: String -> Parser String
reserved s = token (string s)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

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
word = plus $ satisfy (`notElem` " \n\r")

split :: String -> Parser a -> Parser [a]
split t p = do
    first <- p
    v <- star $ string t >> p
    return $ first:v

alpha :: Parser String
alpha = plus (satisfy isAlpha)