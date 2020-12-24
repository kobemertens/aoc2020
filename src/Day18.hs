module Day18
    ( solve
    ) where
-- inspired by this great post:
-- http://dev.stephendiehl.com/fun/002_parsers.html
import NanoParsec
import Control.Applicative
import Control.Monad

data Expr
    = Add Expr Expr
    | Mul Expr Expr
    | Lit Int

eval :: Expr -> Int
eval ex = case ex of
    Add a b -> eval a + eval b
    Mul a b -> eval a * eval b
    Lit n   -> n

int :: Parser Expr
int = Lit <$> number

expr :: Parser Expr
expr = factor expr `chainl1` (mulOp <|> addOp)

expr' :: Parser Expr
expr' = term `chainl1` mulOp

term :: Parser Expr
term = factor expr' `chainl1` addOp

factor :: Parser Expr -> Parser Expr
factor e =
      int
  <|> parens "(" ")" e

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addOp :: Parser (Expr -> Expr -> Expr)
addOp = infixOp "+" Add

mulOp :: Parser (Expr -> Expr -> Expr)
mulOp = infixOp "*" Mul

fileParser :: Parser [Expr]
fileParser = plus $ expr <* spaces

fileParser' :: Parser [Expr]
fileParser' = plus $ expr' <* spaces

solve :: IO ()
solve = do
    f <- filter (/= ' ') <$> readFile "data/day18.txt"
    putStr "Part1: "
    print $ (sum . map eval . runParser fileParser) f
    putStr "Part2: "
    print $ (sum . map eval . runParser fileParser') f