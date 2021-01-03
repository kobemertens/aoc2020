module Main where

import System.Environment ( getArgs )
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16
import qualified Day17
import qualified Day18
import qualified Day19
import qualified Day21
import qualified Day22
import qualified Day23
import qualified Day24

main :: IO ()
main = do
    a <- getArgs
    let daynr = (read . head) a :: Int
    solve daynr

solve :: Int -> IO ()
solve 1  = Day01.solve
solve 2  = Day02.solve
solve 3  = Day03.solve
solve 4  = Day04.solve
solve 5  = Day05.solve
solve 6  = Day06.solve
-- solve 7  = Day07.solve
solve 8  = Day08.solve
solve 9  = Day09.solve
solve 10 = Day10.solve
solve 11 = Day11.solve
solve 12 = Day12.solve
solve 13 = Day13.solve
solve 14 = Day14.solve
solve 15 = Day15.solve
solve 16 = Day16.solve
solve 17 = Day17.solve
solve 18 = Day18.solve
solve 19 = Day19.solve
-- solve 20 = Day20.solve
solve 21 = Day21.solve
solve 22 = Day22.solve
solve 23 = Day23.solve
solve 24 = Day24.solve
-- solve 25 = Day25.solve