module Day19
    ( solve
    ) where

import NanoParsecList
import Data.Map ( Map, fromList, (!) )
import Control.Applicative ( Alternative((<|>), some) )
import Control.Monad (void, MonadPlus(..))
import Debug.Trace

type ParserStore = Map Int StoreEntry
newtype StoreEntry = StoreEntry { getEntry :: ParserStore -> Parser () }

solve :: IO ()
solve = do
    f <- readFile "data/day19.txt"
    let (store, words) = runParser fileParser f
    let first = store!0
    print "Parser was parsed"
    print $ length $ filter (succeeds (getEntry first store)) words
    -- print $ succeeds (getEntry first store) "aaaaabbaabaaaaababaa"

fileParser :: Parser (ParserStore, [String])
fileParser = do
    st <- parserStoreParser
    spaces
    ts <- testStringParser
    spaces
    return (st, ts)

testStringParser :: Parser [String]
testStringParser = some $ alpha <* spaces

parserStoreParser :: Parser ParserStore
parserStoreParser = do
    ns <- some storeEntryParser
    return $ fromList ns

storeEntryParser :: Parser (Int, StoreEntry)
storeEntryParser = do
    n <- number
    string ": "
    lp <- referenceEntryParser <|> litteralEntryParser
    spaces
    return (n, lp)

litteralEntryParser :: Parser StoreEntry
litteralEntryParser = do
    a <- parens "\"" "\"" alpha
    return $ StoreEntry (\_ -> void (string a))

referenceEntryParser :: Parser StoreEntry
referenceEntryParser = do
    l <- split " | " sequenceParser
    return $ anyOf l

sequenceParser :: Parser StoreEntry
sequenceParser = do
    ids <- split " " number
    return $ StoreEntry $ \m ->
        mapM_ (f m) ids
  where
    f :: ParserStore -> Int -> Parser ()
    f m id = getEntry (m!id) m

-- returns succeeding parser
anyOf :: [StoreEntry] -> StoreEntry
anyOf l = StoreEntry $ \m ->
    foldl1 mplus (map (`getEntry` m) l)