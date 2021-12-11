{-# LANGUAGE OverloadedStrings #-}
module Day11 (main) where

import Data.Text ( pack, Text )
import Data.Void ( Void )
import Text.Megaparsec ( parse, many, sepBy, Parsec )
import Text.Megaparsec.Char ( digitChar, newline )

----- Data types -----
type Index  = (Int, Int)

----- Parsing -----
type Parser = Parsec Void Text

line :: Parser [Int]
line = do
    ns <- many digitChar
    return (map (read . pure :: Char -> Int) ns)

parseInput :: String -> [[Int]]
parseInput file = case parse (sepBy line newline) "" (pack file) of
    Right lines -> lines
    _           -> undefined

----- Solution -----
resetFlashed :: [[Bool]]
resetFlashed = [[False | x <- [0..9]] | y <- [0..9]]

toggleFlashed :: Index -> [[Bool]] -> [[Bool]]
toggleFlashed i flashed = [[((x, y) == i) || (flashed !! y !! x) | x <- [0..9]] | y <- [0..9]]

numFlashed :: [[Bool]] -> Int 
numFlashed flashed = sum [fromEnum (flashed !! y !! x) | x <- [0..9], y <- [0..9]]

adjacentIndexes :: Index -> [Index]
adjacentIndexes (x, y) = [(x', y') |
   x' <- [x-1..x+1],
   x' >= 0, x' <= 9,
   y' <- [y-1..y+1],
   y' >= 0, y' <= 9,
   (x', y') /= (x, y)]

allIndexes :: [Index]
allIndexes = [(x, y) | x <- [0..9], y <- [0..9]]

incAllLevels :: [[Int]] -> [[Int]]
incAllLevels levels = [[(levels !! y !! x) + 1 | x <- [0..9]] | y <- [0..9]]

incLevels :: [Index] -> [[Int]] -> [[Int]]
incLevels inds levels = [[if (x, y) `elem` inds then v + 1 else v | x <- [0..9], let v = levels !! y !! x] | y <- [0..9]]


zeroEnergyLevels :: [[Int]] -> [[Bool]] -> [[Int]]
zeroEnergyLevels levels flashed = [[if flashed !! y !! x then 0 else levels !! y !! x | x <- [0..9]] | y <- [0..9]]

checkFlashes :: [Index] -> [[Int]] -> [[Bool]] -> ([[Int]], [[Bool]])
checkFlashes [] levels flashed = (levels, flashed)
checkFlashes ((x,y):rest) levels flashed
    | not f && l > 9 = checkFlashes (rest ++ adjacentIndexes (x,y)) levels' flashed'
    | otherwise      = checkFlashes rest levels flashed
        where
            l        = levels !! y !! x
            f        = flashed !! y !! x
            flashed' = toggleFlashed (x,y) flashed
            levels'  = incLevels (adjacentIndexes (x,y)) levels

step :: [Index] -> [[Int]] -> [[Bool]] -> ([[Int]], Int)
step [] levels flashed       = (zeroEnergyLevels levels flashed, numFlashed flashed)
step (i:rest) levels flashed = step rest levels' flashed'
    where
        (levels', flashed') = checkFlashes [i] levels flashed

-- returns the number of flashes after n steps
simulate :: Int -> Int -> [[Int]] -> Int
simulate 0 acc levels = acc
simulate n acc levels = simulate (n-1) (acc + nf) levels'
    where
        (levels', nf) = step allIndexes (incAllLevels levels) resetFlashed

-- returns the step at which n simulataneous flashes occur
simulateUntil :: Int -> Int -> [[Int]] -> Int
simulateUntil n curStep levels
    | n == nf   = curStep
    | otherwise = simulateUntil n (curStep + 1) levels'
    where
        (levels', nf) = step allIndexes (incAllLevels levels) resetFlashed

part1 :: [[Int]] -> Int
part1 = simulate 100 0

part2 :: [[Int]] -> Int
part2 = simulateUntil 100 1

main :: IO()
main = do
    input <- readFile "data/day11.data"
    putStrLn "Day 11"
    putStr "\tPart 1: "; print (part1 $ parseInput input)
    putStr "\tPart 2: "; print (part2 $ parseInput input)
    