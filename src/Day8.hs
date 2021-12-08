{-# LANGUAGE OverloadedStrings #-}
module Day8 (main, entry) where

import Data.Text ( pack, replace, Text )
import Data.Map as Map (Map, insert, empty, insertWith, fromListWith, toList, (!), fromList )
import Data.Set as Set (Set, fromList, difference, filter, elemAt, toList, delete, map)
import Data.Void (Void)
import Text.Megaparsec ( parse, sepBy, some, Parsec )
import Text.Megaparsec.Char ( hspace, letterChar, newline )

----- Data types -----
type Entry  = ([SegSet], [SegSet])  -- (inputs, outputs)
type SegSet = Set Char      -- set of 'segments'
type SegMap = Map Char Char -- maps the observed segment to actual

displaySegments :: Int -> SegSet
displaySegments i = case i of
    0 -> Set.fromList ['a', 'b', 'c', 'e', 'f', 'g']
    1 -> Set.fromList ['c', 'f']
    2 -> Set.fromList ['a', 'c', 'd', 'e', 'g']
    3 -> Set.fromList ['a', 'c', 'd', 'f', 'g']
    4 -> Set.fromList ['b', 'c', 'd', 'f']
    5 -> Set.fromList ['a', 'b', 'd', 'f', 'g']
    6 -> Set.fromList ['a', 'b', 'd', 'e', 'f', 'g']
    7 -> Set.fromList ['a', 'c', 'f']
    8 -> Set.fromList ['a', 'b', 'c', 'd', 'e', 'f', 'g']
    9 -> Set.fromList ['a', 'b', 'c', 'd', 'f', 'g']
    _ -> undefined

----- Parsing -----
type Parser = Parsec Void Text

entry :: Parser Entry
entry = do
    i <- sepBy (some letterChar) hspace
    let
        (inputs, outputs) = splitAt 10 i
        inSet  = [Set.fromList x | x <- inputs]
        outSet = [Set.fromList x | x <- outputs]
    return (inSet, outSet)

parseInput :: String -> [Entry]
parseInput file = do
    case parse (sepBy entry newline) "" (replace " | " " " (pack file)) of
        Right entries -> entries
        _             -> undefined
    
----- Solution -----
uniqueLengths :: [Int]
uniqueLengths = [2, 3, 4, 7]

filterSegsLen :: Int -> [SegSet] -> [SegSet]
filterSegsLen n = Prelude.filter (\s -> length s == n)

addCountsToMap :: [Char] -> Map Char Int -> Map Char Int
addCountsToMap [] counts     = counts
addCountsToMap (c:cs) counts = addCountsToMap cs counts'
    where
        counts' = insertWith (+) c 1 counts

invert :: (Ord a, Ord b) => Map a b -> Map b [a]
invert m = Map.fromListWith (++) pairs
    where
        pairs = [(v, [k]) | (k, v) <- Map.toList m]


segCounts :: [SegSet] -> (Map Char Int, Map Int [Char])
segCounts s = (cnts, invert cnts) 
    where
        cnts = segCounts' s Map.empty
        segCounts' :: [SegSet] -> Map Char Int -> Map Char Int
        segCounts' [] cnts     = cnts
        segCounts' (seg:ss) cnts = segCounts' ss (addCountsToMap (Set.toList seg) cnts)

removeSeg :: Char -> [Char] -> [Char]
removeSeg r cs = [c | c <- cs, c/=r]

segEncoding :: Entry -> SegMap
segEncoding (inputs, _) = do
    let encoding  = Map.empty
        -- 'a' encoding from unique lengths
        encodingA = Map.insert 'a' (elemAt 0 (difference seven one)) encoding
        -- 'b', 'e', 'f' encodings from character counts
        encodingB = Map.insert 'b' (head $ revCounts ! 6) encodingA
        encodingE = Map.insert 'e' (head $ revCounts ! 4) encodingB
        encodingF = Map.insert 'f' (head $ revCounts ! 9) encodingE
        -- 'c' encoding from character count and above encodings
        encodingC = Map.insert 'c' (head $ removeSeg (encodingA ! 'a') (revCounts ! 8)) encodingF
        -- 'd' encoding from unique lengths (4 & 1) and above encoding for 'b'
        encodingD = Map.insert 'd' (elemAt 0 (delete (encodingB ! 'b') (difference four one))) encodingC
        encodingG = Map.insert 'g' (head $ removeSeg (encodingD ! 'd') (revCounts ! 7)) encodingD
    encodingG
    where
        -- Unique length
        one          = head $ filterSegsLen 2 inputs
        seven        = head $ filterSegsLen 3 inputs
        four         = head $ filterSegsLen 4 inputs
        -- Segment counts
        (counts, revCounts) = segCounts inputs

segPatterns :: Map Char Char -> Map SegSet Int
segPatterns encoding = Map.fromList [(s, n) | n <- [0..9], let s = Set.fromList (Prelude.map (encoding !) (Set.toList $ displaySegments n))]

decode :: Entry -> Int
decode e@(inputs, outputs) = sum [10^(3-i) * n | (i, n) <- zip [0..] outs]
    where
        decoding = segPatterns (segEncoding e)
        outs     = [decoding ! o | o <- outputs]

part1 :: [Entry] -> Int
part1 [] = 0
part1 ((_, outs):es) = n + part1 es
    where
        n = length $ Prelude.filter (`elem` uniqueLengths) [length o | o <- outs]

part2 :: [Entry] -> Int
part2 es = sum $ Prelude.map decode es

main :: IO()
main = do
    file <- readFile "data/day8.data"
    putStrLn "Day 8"
    putStr "\tPart 1: "; print (part1 $ parseInput file)
    putStr "\tPart 2: "; print (part2 $ parseInput file)
    