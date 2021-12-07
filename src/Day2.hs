module Day2 (main) where

import Data.Text
import Data.Void (Void)
import Text.Megaparsec (parse, Parsec, some, sepBy)
import Text.Megaparsec.Char (digitChar, letterChar, hspace1, newline)

----- Data types -----
data Command = Forward Int | Down Int | Up Int

----- Parsing -----
type Parser = Parsec Void Text

command :: Parser Command
command = do
    cmd <- some letterChar
    _ <- hspace1 
    x <- some digitChar
    case cmd of
        "forward" -> return (Forward (read x))
        "up"      -> return (Up (read x))
        "down"    -> return (Down (read x))
        _         -> undefined

inputParser :: Parser [Command]
inputParser = sepBy command newline

parseInput :: String -> [Command]
parseInput file =
    case parse inputParser "" (pack file) of
        Right cmds -> cmds
        _          -> undefined

----- Solution -----
part1 :: [Command] -> Int
part1 cmds = part1' cmds 0 0
    where
        part1' [] h d = h * d
        part1' (c:cs) h d = case c of
            Forward x -> part1' cs (h+x) d
            Down x    -> part1' cs h (d+x)
            Up x      -> part1' cs h (d-x)

part2 :: [Command] -> Int 
part2 cmds = part2' cmds 0 0 0
    where
        part2' [] _ h d = h * d
        part2' (c:cs) a h d = case c of
            Forward x -> part2' cs a (h+x) (d+(a*x))
            Down x    -> part2' cs (a+x) h d
            Up x      -> part2' cs (a-x) h d

main :: IO ()
main = do
    -- Parsec.parse commandParser "" "forward 1"
    input <- readFile "data/day2.data"
    putStrLn "Day 2"
    putStr "\tPart 1: "; print (part1 (parseInput input))
    putStr "\tPart 2: "; print (part2 (parseInput input))
