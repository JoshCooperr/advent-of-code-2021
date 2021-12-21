{-# LANGUAGE OverloadedStrings #-}
module Day15 (main) where

import Data.Text ( pack, Text )
import Data.Void ( Void )
import Data.Set as S ( Set, difference, toList, fromList, notMember, member, empty, insert )
import Data.Map as M ( Map, toList, fromList, lookup, insert )
import Data.List (sortBy, minimumBy)
import Data.Maybe (fromJust)
import Text.Megaparsec ( parse, sepBy, some, Parsec )
import Text.Megaparsec.Char ( digitChar, newline )
import Data.Ord

----- Data types -----
type Coord  = (Int, Int)
type Matrix = [[Int]]

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
initMap :: Matrix -> Map Coord Int
initMap m = M.fromList [if x == 0 && y == 0 then ((x,y), 0) else ((x,y), 10000000) | x <- [0..(length (head m) - 1)], y <- [0..length m - 1]]

neighbours :: Matrix -> Coord -> [Coord]
neighbours m (x, y) = x1 ++ x2 ++ y1 ++ y2
    where
        mX = length (head m) - 1
        mY = length m - 1
        x1 = [(x-1, y) | x /= 0]
        x2 = [(x+1, y) | x < mX]
        y1 = [(x, y-1) | y /= 0]
        y2 = [(x, y+1) | y < mY]

targetCoord :: Matrix -> Coord
targetCoord m = (length (head m) - 1, length m - 1)

-- Get the coord with the smallest distance that is not in the set
getSmallest :: Map Coord Int -> Set Coord -> Coord
getSmallest dists vs = fst $ minimumBy (comparing snd) (filter (\x -> fst x `notMember` vs) (M.toList dists))

visit :: Matrix -> Coord -> [Coord] -> Map Coord Int -> Map Coord Int 
visit m start [] dists     = dists
visit m start ((x, y):cs) dists = visit m start cs dists'
    where
        minD   = min (fromJust (M.lookup start dists) + m !! y !! x) (fromJust (M.lookup (x, y) dists))
        dists' = M.insert (x, y) minD dists

step :: Matrix -> Coord -> Set Coord -> Map Coord Int -> (Set Coord, Map Coord Int)
step m cur vs dists = (vs', dists')
    where
        toVisit = S.difference (S.fromList $ neighbours m cur) vs
        dists'  = visit m cur (S.toList toVisit) dists
        vs'     = S.insert cur vs

dijkstra :: Matrix -> Coord -> Coord -> Set Coord -> Map Coord Int -> Int
dijkstra m cur target vs dists
    | S.member target vs = fromJust $ M.lookup target dists
    | otherwise          = dijkstra m next target vs' dists' 
    where
        (vs', dists') = step m cur vs dists
        next          = getSmallest dists' vs'

part1 :: Matrix -> Int
part1 m = dijkstra m (0,0) (targetCoord m) S.empty (initMap m)

part2 :: Matrix -> Int 
part2 m = dijkstra m' (0,0) (targetCoord m') S.empty (initMap m')
    where
        mX = length (head m)
        mY = length m
        m' = [[(v + (x `div` mX) + (y `div` mY) - 1) `mod` 9 + 1 | x <- [0..mX*5 - 1], let v = m !! (y `mod` mY) !! (x `mod` mX)] | y <- [0..mY*5 - 1]]

main :: IO()
main = do
    input <- readFile "data/day15.data"
    putStrLn "Day 15"
    -- putStr "\tPart 1: "; print (part1 $ parseInput input)
    putStr "\tPart 2: "; print (part2 $ parseInput input)
    