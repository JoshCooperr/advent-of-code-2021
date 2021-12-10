{-# LANGUAGE OverloadedStrings #-}
module Day10 where

import Data.Text ( Text, pack )
import Data.Void ( Void )
import Data.Maybe ( isNothing )
import Data.List ( sort )
import Text.Megaparsec ( Parsec, satisfy, sepBy, parse, many )
import Text.Megaparsec.Char ( newline )

----- Data types -----
newtype Stack a = Stack [a] deriving (Show, Eq)

newStack :: Stack a
newStack = Stack []

push :: a -> Stack a -> Stack a
push x (Stack xs) = Stack (x:xs)

pop :: Stack a -> (Maybe a, Stack a)
pop (Stack [])     = (Nothing, Stack [])
pop (Stack (x:xs)) = (Just x, Stack xs)

----- Parsing -----
type Parser = Parsec Void Text

inputParser :: Parser [String]
inputParser = sepBy (many bracket) newline
    where
        bracket = satisfy (`elem` ("()[]{}<>" :: String))

parseInput :: String -> [String]
parseInput file = do
    case parse inputParser "" (pack file) of
        Right lines -> lines
        Left _      -> undefined

----- Solution -----
bracketMatches :: Char -> Char -> Bool
bracketMatches '(' ')' = True
bracketMatches '[' ']' = True
bracketMatches '{' '}' = True
bracketMatches '<' '>' = True
bracketMatches _ _     = False

validate :: String -> (Maybe Char, Stack Char)
validate = validate' newStack
    where
        validate' :: Stack Char -> String -> (Maybe Char, Stack Char)
        validate' st@(Stack cs) [] = case cs of
            [] -> (Nothing, st) -- valid
            _  -> (Nothing, st) -- incomplete
        validate' stack (b:bs)
            | b `elem` ("([{<" :: String) = validate' (push b stack) bs
            | b `elem` (")]}>" :: String) = case p of
                Nothing -> (Nothing, stack) -- incomplete
                Just c  -> if bracketMatches c b
                    then validate' st bs
                    else (Just b, stack)
            | otherwise = undefined
            where
                (p, st) = pop stack

missingBrackets :: Stack Char -> String
missingBrackets (Stack []) = []
missingBrackets stack = case pop stack of
    (Just '(', st) -> ')' : missingBrackets st
    (Just '[', st) -> ']' : missingBrackets st
    (Just '{', st) -> '}' : missingBrackets st
    (Just '<', st) -> '>' : missingBrackets st
    _              -> undefined

errorScore :: Maybe Char -> Int
errorScore (Just ')') = 3
errorScore (Just ']') = 57
errorScore (Just '}') = 1197
errorScore (Just '>') = 25137
errorScore Nothing    = 0
errorScore _          = undefined

autocompleteScore :: String -> Int
autocompleteScore bs = foldl (\t s -> t * 5 + s) 0 [score b | b <- bs]
    where
        score :: Char -> Int
        score ')' = 1
        score ']' = 2
        score '}' = 3
        score '>' = 4
        score _   = undefined


part1 :: [String] -> Int
part1 lines = sum [errorScore v | (v, _) <- vs]
    where
        vs = [validate l | l <- lines]

part2 :: [String] -> Int
part2 lines = sort [autocompleteScore $ missingBrackets st | st <- stacks] !! i
    where
        validated = [validate l | l <- lines]
        stacks    = [st | (v, st) <- validated, isNothing v, st /= Stack []]
        i         = (length stacks - 1) `div` 2

main :: IO()
main = do
    input <- readFile "data/day10.data"
    putStrLn "Day 10"
    putStr "\tPart 1: "; print (part1 $ parseInput input)
    putStr "\tPart 2: "; print (part2 $ parseInput input)
    