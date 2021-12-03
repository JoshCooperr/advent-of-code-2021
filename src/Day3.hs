module Day3 where

import Data.List(transpose)
import Data.Char(digitToInt)

mostCommonBit :: String -> Char
mostCommonBit num =
    if length (filter (== '1') num) >= length (filter (== '0') num) then '1' else '0'

invertBit :: Char -> Char 
invertBit b = case b of
    '1' -> '0'
    '0' -> '1'
    _   -> undefined 

invertBits :: String -> String
invertBits = map invertBit

gamma :: [String] -> String
gamma bins = map mostCommonBit (transpose bins)

epsilon :: [String] -> String
epsilon bins = invertBits (gamma bins)

toDecimal :: String -> Int
toDecimal = foldl (\acc x -> acc * 2 + digitToInt x) 0

part1 :: [String] -> Int 
part1 bins = e * g
    where
        e = toDecimal (epsilon bins)
        g = toDecimal (gamma bins)

-- Binary nums, Transposed nums, Bit index -> Binary num (oxygen rating)
oxygenRating :: [String] -> [String] -> Int -> String
oxygenRating [num] _ _   = num
oxygenRating nums bits i = oxygenRating valid (transpose valid) (i + 1)
    where
        x     = mostCommonBit (bits !! i)
        valid = filter (\n -> n !! i == x) nums

-- Binary nums, Transposed nums, Bit index -> Binary num (co2 rating)
co2Rating :: [String] -> [String] -> Int -> String
co2Rating [num] _ _   = num
co2Rating nums bits i = co2Rating valid (transpose valid) (i + 1)
    where
        x     = invertBit (mostCommonBit (bits !! i))
        valid = filter (\n -> n !! i == x) nums

part2 :: [String] -> Int
part2 bins = o * c
    where
        o = toDecimal (oxygenRating bins (transpose bins) 0)
        c = toDecimal (co2Rating bins (transpose bins) 0)

main :: IO()
main = do
    input <- readFile "data/day3.data"
    putStr "Part 1: "
    print (part1 (lines input))
    putStr "Part 2: "
    print (part2 (lines input))
