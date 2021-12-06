{-# LANGUAGE OverloadedStrings #-}
module Day5 where

import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

----- Types -----
type Coordinate = (Int, Int)
type Line       = (Coordinate, Coordinate)
type Diagram    = [[Int]]

----- Parsing -----
type Parser = Parsec Void Text

coord :: Parser Coordinate
coord = do
    x <- some digitChar
    _ <- char ','
    y <- some digitChar
    return (read x, read y)

line :: Parser Line
line = do
    start <- coord
    _ <- string " -> "
    end <- coord
    return (start, end)

inputParser :: Parser [Line]
inputParser = sepBy line (char '\n')

parseInput :: String -> [Line]
parseInput file =
    case parse inputParser "" (pack file) of
        Right lines -> lines
        _           -> undefined

----- Solution -----
isVertical :: Line -> Bool
isVertical ((x1, _), (x2, _)) = x1 == x2

isHorizontal :: Line -> Bool
isHorizontal ((_, y1), (_, y2)) = y1 == y2

-- This can probably be done with a foldr?
replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = (f x):xs
replace f i (x:xs) = x : replace f (i-1) xs
replace _ _ [] = []

replace2D :: (a -> a) -> (Int, Int) -> [[a]] -> [[a]]
replace2D f (x,y) = replace (replace f y) x

crossCoord :: Coordinate -> Diagram -> Diagram
crossCoord c@(x, y) d = replace2D (const (v + 1)) c d
    where
        v = (d !! x) !! y

lineCoords :: Line -> [Coordinate]
lineCoords ((x1, y1), (x2, y2))
    | x1 == x2               = zip (repeat x1) ys  -- Vertical
    | y1 == y2               = zip xs (repeat y1)  -- Horizontal
    | (x1 < x2) == (y1 < y2) = zip xs ys           -- Diagonal (Down/Right)
    | otherwise              = zip xs (reverse ys) -- Diagonal (Up/Right)
    where
        xs = if x2 > x1 then [x1..x2] else [x2..x1]
        ys = if y2 > y1 then [y1..y2] else [y2..y1]

addLine :: Line -> Diagram -> Diagram
addLine l d = addline' coords d
    where
        coords = lineCoords l
        addline' [] d = d
        addline' (c:cs) d = addline' cs (crossCoord c d)

drawLines :: [Line] -> Diagram -> Diagram
drawLines ls d = foldl (flip addLine) d ls

numIntersections :: Diagram -> Int
numIntersections d = length (filter (>1) ns)
    where
        ns = concat d

maxDimensions :: [Line] -> (Int, Int)
maxDimensions = maxDimensions' (0,0)
    where
        maxDimensions' (x, y) [] = (x, y)
        maxDimensions' (x, y) (((x1,y1), (x2,y2)):ls) =
            maxDimensions' (maximum [x, x1, x2], maximum [y, y1, y2]) ls

part1 :: [Line] -> Int
part1 ls = numIntersections (drawLines lines d)
    where
        lines  = filter (\l -> isVertical l || isHorizontal l) ls
        (x, y) = maxDimensions ls
        d      = [[0 | _ <- [0..x]] | _ <- [0..y]]

part2 :: [Line] -> Int
part2 ls = numIntersections (drawLines ls d)
    where
        (x, y) = maxDimensions ls
        d      = [[0 | _ <- [0..x]] | _ <- [0..y]]

main :: IO()
main = do
    file <- readFile "data/day5.data"
    putStr "\tPart 1: "
    print(part1 (parseInput file))
    putStr "\tPart 2: "
    print(part2 (parseInput file))
    