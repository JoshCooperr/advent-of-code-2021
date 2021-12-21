module AoC where

import System.Environment (getArgs)
import Day1 (main)
import Day2 (main)
import Day3 (main)
import Day4 (main)
import Day5 (main)
import Day6 (main)
import Day7 (main)
import Day8 (main)
import Day9 (main)
import Day10 (main)
import Day11 (main)
import Day12 (main)
import Day13 (main)
import Day14 (main)
import Day15 (main)
import Day16 (main)

type Main = (Int, IO())

mains :: [Main]
mains = zip [1..] [
    Day1.main,
    Day2.main,
    Day3.main,
    Day4.main,
    Day5.main,
    Day6.main,
    Day7.main,
    Day8.main,
    Day9.main,
    Day10.main,
    Day11.main,
    Day12.main,
    Day13.main,
    Day14.main,
    Day15.main,
    Day16.main
    ]

main :: IO ()
main = do
    args <- getArgs
    let toRun = if null args then [1..] else map read args
    sequence_ [io | io <- [main | (i, main) <- mains, i `elem` toRun]]
