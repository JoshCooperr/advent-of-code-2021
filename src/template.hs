module DayX (main) where

import Data.Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

----- Data types -----

----- Parsing -----
type Parser = Parsec Void Text

----- Solution -----

part1 :: a -> Int 
part1 _ = 0

part2 :: a -> Int 
part2 _ = 0

main :: IO()
main = do
    input <- readFile "data/dayX.sample.data"
    putStrLn "Day X"
    putStr "\tPart 1: "; print (part1 0)
    putStr "\tPart 2: "; print (part2 0)
    