{-# LANGUAGE OverloadedStrings #-}
module Day16 (main) where

import Data.Text ()
import Data.Void ( Void )
import Data.List ( foldl )
import Data.Char (digitToInt)

----- Data Types -----
type Packet = (Int, Int, PacketValue)
data PacketValue = Literal String | Sub [Packet] deriving (Show)

----- Solution -----
hexToBin :: String -> String
hexToBin [] = ""
hexToBin (h:rest) = case h of
    '0' -> "0000" ++ hexToBin rest
    '1' -> "0001" ++ hexToBin rest
    '2' -> "0010" ++ hexToBin rest
    '3' -> "0011" ++ hexToBin rest
    '4' -> "0100" ++ hexToBin rest
    '5' -> "0101" ++ hexToBin rest
    '6' -> "0110" ++ hexToBin rest
    '7' -> "0111" ++ hexToBin rest
    '8' -> "1000" ++ hexToBin rest
    '9' -> "1001" ++ hexToBin rest
    'A' -> "1010" ++ hexToBin rest
    'B' -> "1011" ++ hexToBin rest
    'C' -> "1100" ++ hexToBin rest
    'D' -> "1101" ++ hexToBin rest
    'E' -> "1110" ++ hexToBin rest
    'F' -> "1111" ++ hexToBin rest
    _   -> undefined

binToInt :: String -> Int
binToInt = foldl (\acc x -> acc * 2 + digitToInt x) 0

extractLiteral :: String -> (String, String)
extractLiteral = extractLiteral' ""
    where
        extractLiteral' lit ('0':b1:b2:b3:b4:bs) = (lit ++ [b1,b2,b3,b4], bs)
        extractLiteral' lit ('1':b1:b2:b3:b4:bs) = extractLiteral' (lit ++ [b1,b2,b3,b4]) bs
        extractLiteral' _ _ = undefined

packetLength :: Packet -> Int
packetLength (_, _, Literal n) = 6 + length n
packetLength (_, _, Sub ps)    = sum (6 : [packetLength p | p <- ps])

decodeSubpacketsLen :: Int -> String -> [Packet] -> ([Packet], String)
decodeSubpacketsLen 0 bs ps = (ps, bs)
decodeSubpacketsLen l bs ps
    | length bs < 11 = (ps, bs)
    | otherwise      = decodeSubpacketsLen (l - packetLength p) rest (ps ++ [p])
    where
        (p, rest) = decodePacket bs

decodeSubpacketsNum :: Int -> String -> [Packet] -> ([Packet], String)
decodeSubpacketsNum 0 bs ps = (ps, bs)
decodeSubpacketsNum n bs ps
    | length bs < 11 = (ps, bs)
    | otherwise      = decodeSubpacketsNum (n-1) rest (ps ++ [p])
    where
        (p, rest) = decodePacket bs

decodePacket :: String -> (Packet, String)
decodePacket (v1:v2:v3:t1:t2:t3:rest) = case typeId of
    4 -> ((version, typeId, Literal n), remaining)
        where
            (n, remaining) = extractLiteral rest
    _ -> case l of
        '0' -> let
                (tmp, ps) = splitAt 15 bs
                len       = binToInt tmp
                (subPacks, remaining) = decodeSubpacketsLen len ps []
            in
                ((version, typeId, Sub subPacks), remaining)
        '1' -> let
                (tmp, ps) = splitAt 11 bs
                num       = binToInt tmp
                (subPacks, remaining) = decodeSubpacketsNum num ps []

            in
                ((version, typeId, Sub subPacks), remaining)
        _   -> undefined
    where
        version = binToInt [v1, v2, v3]
        typeId  = binToInt [t1, t2, t3]
        l:bs    = rest
decodePacket _ = undefined

sumValues :: Packet -> Int
sumValues (v, _, Literal _) = v
sumValues (v, _, Sub ps)    = v + sum [sumValues p | p <- ps]

evaluate :: Packet -> Int
evaluate (_, _, Literal n) = binToInt n
evaluate (_, 0, Sub ps) = sum $ map evaluate ps
evaluate (_, 1, Sub ps) = product $ map evaluate ps
evaluate (_, 2, Sub ps) = minimum $ map evaluate ps
evaluate (_, 3, Sub ps) = maximum $ map evaluate ps
evaluate (_, 5, Sub [p1, p2]) = if evaluate p1 > evaluate p2 then 1 else 0
evaluate (_, 6, Sub [p1, p2]) = if evaluate p1 < evaluate p2 then 1 else 0
evaluate (_, 6, Sub ps) = undefined
evaluate (_, 7, Sub ps) = if evaluate (head ps) == evaluate (ps !! 1) then 1 else 0

getPacket :: String -> Packet
getPacket hex = packet
    where
        (packet, _) = decodePacket (hexToBin hex)

part1 :: String -> Int
part1 = sumValues . getPacket

part2 :: String -> Int
part2 = evaluate . getPacket

main :: IO()
main = do
    input <- readFile "data/day16.data"
    putStrLn "Day 16"
    print (getPacket input)
    putStr "\tPart 1: "; print (part1 input)
    putStr "\tPart 2: "; print (part2 input)
    