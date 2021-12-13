{-# LANGUAGE OverloadedStrings #-}
module Day12 (main) where

import Data.Text ( pack, Text )
import Data.Void ( Void )
import Data.List ( sortBy )
import Data.Map as M ( Map, adjust, empty, insert, member, lookup, elems, fromList, keys )
import Data.Set as S ( Set, fromList, insert, empty, toList )
import Text.Megaparsec ( parse, many, sepBy, Parsec )
import Text.Megaparsec.Char ( char, letterChar, newline )
import Data.Char (isUpper)

----- Data types -----
type Node  = String
type Edge  = (Node, Node)
type Graph = Map Node (Set Node)
type Path  = [Node]

----- Parsing -----
type Parser = Parsec Void Text

edge :: Parser Edge
edge = do
    a <- many letterChar
    _ <- char '-'
    b <- many letterChar
    return (a, b)

edges :: Parser [Edge]
edges = sepBy edge newline

parseInput :: String -> Graph
parseInput file = do
    case parse edges "" (pack file) of
        Right es -> buildGraph es (M.empty :: Map Node (Set Node))
        _        -> undefined
    where
        buildGraph :: [Edge] -> Graph -> Graph
        buildGraph [] g         = g
        buildGraph ((s,d):es) g = buildGraph es g''
            where
                g'  = if M.member s g then M.adjust (S.insert d) s g else M.insert s (S.fromList [d]) g
                g'' = if M.member d g' then M.adjust (S.insert s) d g' else M.insert d (S.fromList [s]) g'

----- Solution -----
isBig :: Node -> Bool
isBig = all isUpper

canVisit :: Node -> Int -> Map Node Int -> Bool
canVisit "start" _ _ = False 
canVisit n 1 cnts
    | isBig n             = True
    | otherwise           = maximum (M.elems cnts) <= 1
canVisit n limit cnts
    | isBig n        = True
    | length cs == 1 = head cs <= limit
    | otherwise      = head cs <= limit && head (tail cs) < limit
        where
            cs = sortBy (flip compare) (M.elems cnts)

findPaths :: Node -> Node -> Int -> Graph -> [Path]
findPaths start dest limit g = explore start M.empty
    where
        explore :: Node -> Map Node Int -> [Path]
        explore cur cnts
            | cur == dest = [[dest]]
            | otherwise   = case M.lookup cur g of
                Nothing   -> return []
                Just next -> [cur:path | n <- S.toList next, canVisit n limit cnts', path <- explore n cnts']
                where
                    cnts'
                        | isBig cur         = cnts
                        | M.member cur cnts = M.adjust (+1) cur cnts
                        | otherwise         = M.insert cur 1 cnts


part1 :: Graph -> Int
part1 g = length $ findPaths "start" "end" 1 g

part2 :: Graph -> Int
part2 g = length $ findPaths "start" "end" 2 g

main :: IO()
main = do
    input <- readFile "data/day12.data"
    putStrLn "Day 12"
    print (findPaths "start" "end" 1 (parseInput input))
    putStr "\tPart 1: "; print (part1 $ parseInput input)
    putStr "\tPart 2: "; print (part2 $ parseInput input)
    