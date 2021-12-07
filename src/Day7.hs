module Day7 (main) where

import Data.Text ( Text, pack )
import Data.Void (Void)
import Text.Megaparsec ( parse, sepBy, some, Parsec )
import Text.Megaparsec.Char ( char, digitChar )

----- Parsing -----
type Parser = Parsec Void Text

inputParser :: Parser [Int]
inputParser = do
    nums <- sepBy (some digitChar) (char ',')
    return (map read nums)

parseInput :: String -> [Int]
parseInput file = do
    case parse inputParser "" (pack file) of
        Right ns  -> ns
        _         -> undefined

----- Solution -----
fuelCost :: (Int -> Int -> Int) -> [Int] -> Int -> Int 
fuelCost cost ns t = sum [cost n t | n <- ns]

part1 :: [Int] -> Int 
part1 ns = minimum [fuelCost cost ns t | t <- [minN..maxN]]
    where
        cost a b = abs (a-b)
        minN     = minimum ns
        maxN     = maximum ns

part2 :: [Int] -> Int 
part2 ns = minimum [fuelCost cost ns t | t <- [minN..maxN]]
    where
        cost a b = sum [1..(abs (a-b))]
        minN     = minimum ns
        maxN     = maximum ns

main :: IO()
main = do
    input <- readFile "data/day7.data"
    putStrLn "Day 7"
    putStr "\tPart 1: "; print (part1 $ parseInput input)
    putStr "\tPart 2: "; print (part2 $ parseInput input)
    