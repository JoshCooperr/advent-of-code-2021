{-# LANGUAGE OverloadedStrings #-}
module Day13 (main, inputParser) where

import Data.Text ( pack, Text )
import Data.Void ( Void )
import Data.List ( intercalate )
import Text.Megaparsec ( parse, satisfy, many, sepEndBy, Parsec )
import Text.Megaparsec.Char ( char, digitChar, string )

----- Data types -----
type Coord = (Int, Int)
type Paper = [[Bool]]
type Instr = (Char, Int)

----- Parsing -----
type Parser = Parsec Void Text

coord :: Parser Coord
coord = do
    x <- many digitChar
    _ <- char ','
    y <- many digitChar
    return (read x, read y)

instr :: Parser Instr
instr = do
    _    <- string "fold along "
    axis <- satisfy (`elem` ['x', 'y'])
    _    <- char '='
    n    <- many digitChar
    return (axis, read n)

inputParser :: Parser ([Coord], [Instr])
inputParser = do
    coords <- sepEndBy coord (char '\n')
    _ <- char '\n'
    instrs <- sepEndBy instr (char '\n')
    return (coords, instrs)

parseInput :: String -> ([Coord], [Instr])
parseInput file = case parse inputParser "" (pack file) of
    Right (cs, is) -> (cs, is)
    _              -> undefined

----- Solution -----
replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = f x : xs
replace f i (x:xs) = x : replace f (i-1) xs
replace _ _ [] = []

replace2D :: (a -> a) -> (Int, Int) -> [[a]] -> [[a]]
replace2D f (x,y) = replace (replace f x) y

setupPaper :: [Coord] -> Paper
setupPaper coords = drawDots coords fresh
    where
        (xs, ys) = unzip coords
        fresh    = [[False | x <- [0..maximum xs]] | y <- [0..maximum ys]]
        drawDots :: [Coord] -> Paper -> Paper
        drawDots [] p = p
        drawDots (c:cs) p = drawDots cs (replace2D (const True) c p)

countDots :: Paper -> Int
countDots p = sum [fromEnum b | x <- [0..length (head p) - 1], y <- [0..length p - 1], let b = p !! y !! x]

foldPaper :: Instr -> Paper -> Paper
foldPaper ('y', f) p = [[b1 || b2 | x <- [0..length (head p) - 1], let b1 = p !! y !! x, let b2 = p !! (f - (y - f)) !! x] | y <- [0..f-1]]
foldPaper ('x', f) p = [[b1 || b2 | x <- [0..f - 1], let b1 = p !! y !! x, let b2 = p !! y !! (f - (x - f))] | y <- [0..length p - 1]]
foldPaper _ _        = undefined

draw :: Paper -> String
draw p = intercalate "\n" (map drawRow p)
    where
        drawRow :: [Bool] -> String
        drawRow row = [if b then '#' else ' ' | x <- [0..length row - 1], let b = row !! x]

foldAll :: [Instr] -> Paper -> Paper
foldAll is p = foldl (flip foldPaper) p is

part1 :: ([Coord], [Instr]) -> Int
part1 (cs, is) = countDots $ foldPaper (head is) (setupPaper cs)

part2 :: ([Coord], [Instr]) -> String
part2 (cs, is) = draw $ foldAll is (setupPaper cs)

main :: IO()
main = do
    input <- readFile "data/day13.data"
    putStrLn "Day 13"
    putStr "\tPart 1: "; print (part1 $ parseInput input)
    putStrLn "\tPart 2: "; putStrLn (part2 $ parseInput input)
    