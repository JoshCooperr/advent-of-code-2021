{-# LANGUAGE OverloadedStrings #-}
module Day9 (main) where

import Data.Text ( Text, pack )
import Data.Void ( Void )
import Data.List ( nub, sortBy, sort )
import Text.Megaparsec ( Parsec, parse, sepBy, some )
import Text.Megaparsec.Char ( digitChar, newline )

----- Data types -----
type Coord    = (Int, Int)     -- (x, y)
type Matrix   = [[Int]]        -- 2d array of heights
type BasinMap = [(Coord, Int)] -- [(lowest point in basin, size)]


----- Parsing -----
type Parser = Parsec Void Text

inputParser :: Parser Matrix
inputParser = do
    rows <- sepBy (some digitChar) newline
    return [[read [n] | n <- r] | r <- rows]

parseInput :: String -> Matrix
parseInput file = do
    case parse inputParser "" (pack file) of
        Right matrix -> matrix
        _            -> undefined 

----- Solution -----
-- If returned height == 10 -> coord out of range
adjacentHeights :: Matrix -> Coord -> [(Coord, Int)]
adjacentHeights m (x, y) = [((x, y-1), u), ((x, y+1), d), ((x-1, y), l), ((x+1, y), r)]
    where
        u = if y == 0 then 10 else m !! (y - 1) !! x
        d = if y == (length m - 1) then 10 else m !! (y + 1) !! x
        r = if x == (length (head m) - 1) then 10 else m !! y !! (x + 1)
        l = if x == 0 then 10 else m !! y !! (x - 1)

isLowest :: Matrix -> Coord -> Bool
isLowest m c@(x, y) = and [n < v | (_, v) <- adjacentHeights m c]
    where
        n = m !! y !! x

riskLevel :: Matrix -> Coord -> Int
riskLevel m (x, y)
    | isLowest m (x, y) = (m !! y !! x) + 1
    | otherwise         = 0

-- coord = lowest point in basin
incrementBasinMap :: Coord -> BasinMap -> BasinMap
incrementBasinMap coord [] = [(coord, 1)]
incrementBasinMap coord ((c, n):rest)
    | coord == c = (coord, n+1) : rest
    | otherwise  = (c, n) : incrementBasinMap coord rest

generateBasinMap :: Matrix -> BasinMap
generateBasinMap m = generateBasinMap' m (allCoords m) []
    where
        generateBasinMap' :: Matrix -> [Coord] -> BasinMap -> BasinMap
        generateBasinMap' m [] bm     = bm
        generateBasinMap' m (c:cs) bm = generateBasinMap' m cs (incrementBasinMap (findBasinCoord m c) bm)

step :: Matrix -> Coord -> Coord
step m c@(x, y)
    | null adj  = c
    | n >= 9    = (-1, -1)
    | otherwise = fst $ head adj
    where
        n   = m !! y !! x
        adj = [(a, h) | (a, h) <- adjacentHeights m c, h < n]

findBasinCoord :: Matrix -> Coord -> Coord
findBasinCoord m c
    | c == next        = c
    | next == (-1, -1) = (-1, -1)
    | otherwise        = findBasinCoord m next
    where
        next = step m c

allCoords :: Matrix -> [Coord]
allCoords matrix = [(x, y) | y <- [0..length matrix - 1], x <- [0..length (head matrix) - 1]]

part1 :: Matrix -> Int 
part1 m = sum $ [riskLevel m (x,y) | (x,y) <- allCoords m]

part2 :: Matrix -> Int
part2 m = product $ take 3 $ reverse $ sort [n | (c, n) <- generateBasinMap m, c /= (-1, -1)]

main :: IO()
main = do
    input <- readFile "data/day9.data"
    putStrLn "Day 9"
    putStr "\tPart 1: "; print (part1 $ parseInput input)
    putStr "\tPart 2: "; print (part2 $ parseInput input)
    