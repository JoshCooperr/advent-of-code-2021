{-# LANGUAGE OverloadedStrings #-}
module Day14 (main) where

import Data.Text ( pack, Text )
import Data.Void ( Void )
import Data.Map as M
    ( Map, fromList, fromListWith, lookup, toList )
import Data.List ( minimumBy )
import Data.Function ( on )
import Text.Megaparsec ( parse, many, sepBy, Parsec )
import Text.Megaparsec.Char ( letterChar, newline, space, string )

----- Data types -----
type Rule    = ((Char, Char), Char)
type RuleMap = Map (Char, Char) Char
type PairMap = Map (Char, Char) Int

----- Parsing -----
type Parser = Parsec Void Text

rule :: Parser Rule
rule = do
    a <- letterChar; b <- letterChar; _ <- string " -> "; o <- letterChar
    return ((a, b), o)

inputParser :: Parser (String, [Rule])
inputParser = do
    str <- many letterChar; _ <- space; rules <- sepBy rule newline; return (str, rules)

parseInput :: String -> (String, RuleMap)
parseInput file = case parse inputParser "" (pack file) of
    Right (s, rs) -> (s, fromList rs)
    _             -> undefined

----- Solution -----
mostCommon' :: String -> (Char, Int)
mostCommon' str = minimumBy (flip compare `on` snd) chars
    where
        chars = toList $ fromListWith (+) [(c, 1) | c <- str]

leastCommon' :: String -> (Char, Int)
leastCommon' str = minimumBy (compare `on` snd) chars
    where
        chars = toList $ fromListWith (+) [(c, 1) | c <- str]

-- applyRules :: String -> RuleMap -> String
-- applyRules [] _        = []
-- applyRules (x:"") _    = [x]
-- applyRules (a:b:cs) rm = new ++ applyRules (b:cs) rm
--     where
--         new = case M.lookup (a,b) rm of
--             Just c  -> a : c : ""
--             Nothing -> ""

-- stepN :: Int -> RuleMap -> String -> String
-- stepN 0 rm str = str
-- stepN n rm str = stepN (n-1) rm (applyRules str rm)

--------------------
--------------------

mostCommon :: PairMap -> (Char, Int)
mostCommon pm = minimumBy (flip compare `on` snd) [(c, ceiling (cnt / 2)) | (c, cnt) <- chars]
    where
        as = [(a, fromIntegral cnt) :: (Char, Float) | ((a, _), cnt) <- toList pm]
        bs = [(b, fromIntegral cnt) :: (Char, Float) | ((_, b), cnt) <- toList pm]
        chars = toList $ fromListWith (+) (as ++ bs)

leastCommon :: PairMap -> (Char, Int)
leastCommon pm = minimumBy (compare `on` snd) [(c, ceiling cnt) | (c, cnt) <- chars]
    where
        as = [(a, fromIntegral cnt / 2) :: (Char, Float) | ((a, _), cnt) <- toList pm]
        bs = [(b, fromIntegral cnt / 2) :: (Char, Float) | ((_, b), cnt) <- toList pm]
        chars = toList $ fromListWith (+) (as ++ bs)

step :: RuleMap -> PairMap -> PairMap
step rm pm = fromListWith (+) $ step' rm (toList pm)
    where
        cnts = toList pm
        step' :: RuleMap -> [((Char, Char), Int)] -> [((Char, Char), Int)]
        step' rm [] = []
        step' rm (((a,b), cnt):rest) = case M.lookup (a,b) rm of
            Just c  -> [((a, c), cnt), ((c, b), cnt)] ++ step' rm rest
            Nothing -> ((a, b), cnt) : step' rm rest

stepN :: Int -> RuleMap -> PairMap -> PairMap
stepN 0 _ pm  = pm
stepN n rm pm = stepN (n-1) rm (step rm pm)


pairMap :: String -> PairMap
pairMap str = fromListWith (+) (pairMap' str)
    where
        pairMap' [] = []
        pairMap' (x:"") = []
        pairMap' (a:b:rest) = ((a,b), 1) : pairMap' (b:rest)


part1 :: (String, RuleMap) -> Int
part1 (start, rm) = mc - lc
    where
        str     = stepN 10 rm (pairMap start)
        (_, mc) = mostCommon str
        (_, lc) = leastCommon str

part2 :: (String, RuleMap) -> Int
part2 (start, rm) = mc - lc
    where
        str     = stepN 40 rm (pairMap start)
        (_, mc) = mostCommon str
        (_, lc) = leastCommon str

main :: IO()
main = do
    input <- readFile "data/day14.data"
    putStrLn "Day 14"
    print (mostCommon (pairMap "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCBNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNHHNHNHNHNHNHNNNNNHNHNHNHHHHNNHNHNNNBBDSFGSDFGSDFNNNNNNNNNNNN"))
    print (mostCommon' "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCBNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNNHHNHNHNHNHNHNNNNNHNHNHNHHHHNNHNHNNNBBDSFGSDFGSDFNNNNNNNNNNNN")

    putStr "\tPart 1: "; print (part1 $ parseInput input)
    putStr "\tPart 2: "; print (part2 $ parseInput input)
    