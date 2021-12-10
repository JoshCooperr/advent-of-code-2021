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
    Day10.main
    ]

main :: IO ()
main = do
    args <- getArgs
    let toRun = if null args then [1..] else map read args
    sequence_ [io | io <- [main | (i, main) <- mains, i `elem` toRun]]
