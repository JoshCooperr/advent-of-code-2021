module Day6 (main) where

import Data.Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

----- Data types -----
type Counts = (Int, Int, Int, Int, Int, Int, Int, Int, Int)

----- Parsing -----
type Parser = Parsec Void Text

fish :: Parser [Int] 
fish = do
    fs <- sepBy (some digitChar) (char ',')
    return (Prelude.map read fs)

parseInput :: String -> [Int]
parseInput file = do
    case parse fish "" (pack file) of
        Right fish -> fish
        _          -> undefined

----- Solution -----
simDay :: Counts -> Counts
simDay (c0, c1, c2, c3, c4, c5, c6, c7, c8) =
    (c1, c2, c3, c4, c5, c6, c7 + c0, c8, c0)

simulate :: Counts -> Int -> Counts
simulate cs 0 = cs
simulate cs d = simulate (simDay cs) (d - 1)

counts :: [Int] -> Counts
counts fs = (c 0, c 1, c 2, c 3, c 4, c 5, c 6, c 7, c 8)
    where
        c x = Prelude.length (Prelude.filter (==x) fs)

totalCount :: Counts -> Int 
totalCount (c0, c1, c2, c3, c4, c5, c6, c7, c8) = c0 + c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8

part1 :: [Int] -> Int 
part1 fs = totalCount $ simulate (counts fs) 80

part2 :: [Int] -> Int 
part2 fs = totalCount $ simulate (counts fs) 256

main :: IO()
main = do
    input <- readFile "data/day6.data"
    putStrLn "Day 6"
    putStr "\tPart 1: "
    print (part1 (parseInput input))
    putStr "\tPart 2: "
    print (part2 (parseInput input))
    