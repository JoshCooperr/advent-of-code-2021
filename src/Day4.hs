module Day4 where

import Data.Text
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

-- Board is a list of length 25 (5x5): (number, marked?)
type Board = [(Int, Bool)]

markCell :: (Int, Bool) -> Int -> (Int, Bool)
markCell (num, m) n = (num, num == n || m)

markBoard :: Board -> Int -> Board
markBoard board n = Prelude.map (`markCell` n) board

-- Returns the boolean of a given cell
checkCell :: Board -> Int -> Bool
checkCell board i = m
    where
        (_, m) = board !! i

-- Returns True if all given cells are marked, otherwise False
checkCells :: Board -> [Int] -> Bool 
checkCells board cells = and [checkCell board c | c <- cells]

-- (0, 5, 10, 15, 20) = 0 = 5i + row
-- (1, 6, 11, 16, 21) = 1
checkColumn :: Board -> Int -> Bool
checkColumn board row = checkCells board [i * 5 + row | i <- [0..4]]

-- (0, 1, 2, 3, 4) = 0
-- (5, 6, 7, 8, 9) = 1
checkRow :: Board -> Int -> Bool
checkRow board row = checkCells board [(row * 5)..(row * 5 + 4)]

checkBingo :: Board -> Bool
checkBingo board = or [checkColumn board r | r <- [0..4]] || or [checkRow board r | r <- [0..4]]

boardScore :: Board -> Int -> Int
boardScore b n = sum [if not m then x else 0 | (x, m) <- b] * n

type Parser = Parsec Void Text

bingoNumbers :: Parser [String]
bingoNumbers = do
    sepBy (some digitChar) (char ',')

boardLine :: Parser [String]
boardLine = do
    _ <- many (char ' ')
    sepBy (some digitChar) hspace1

board :: Parser [[String]]
board = do
    sepBy boardLine (char '\n')

newline2 :: Parser Char
newline2 = do _ <- char '\n'; char '\n'

formatBoards :: [[String]] -> [String] -> [Board] -> [Board]
formatBoards (l:ls) cur boards
    | l == []   = formatBoards ls [] (boards ++ [Prelude.map (\x -> (read x, False))cur])
    | otherwise = formatBoards ls (cur ++ l) boards
formatBoards [] _ boards = boards

inputParser :: Parser ([Board], [Int])
inputParser = do
    nums <- bingoNumbers
    _ <- newline2
    boards <- sepBy board newline2
    return (formatBoards (Prelude.head boards) [] [], Prelude.map read nums)

parseInput :: String -> ([Board], [Int])
parseInput file =
    case parse inputParser "" (pack file) of
        Right (boards, nums) -> (boards, nums)
        _                    -> undefined

-- Returns a tuple: (num moves to bingo, score)
playBoard :: Board -> [Int] -> Int -> (Int, Int)
playBoard _ [] t = (100000, t) -- Board did not win
playBoard b (n:ns) t
    | checkBingo b' = (t, boardScore b' n)
    | otherwise     = playBoard b' ns (t + 1)
    where
        b' = markBoard b n

quickestWin :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
quickestWin [] w = w
quickestWin (b:bs) (0, 0) = quickestWin bs b
quickestWin ((t,s):bs) (best_t, best_s)
    | t < best_t = quickestWin bs (t,s)
    | otherwise  = quickestWin bs (best_t, best_s)

slowestWin :: [(Int, Int)] -> (Int, Int) -> (Int, Int)
slowestWin [] w = w
slowestWin (b:bs) (0, 0) = slowestWin bs b
slowestWin ((t,s):bs) (worst_t, worst_s)
    | t > worst_t = slowestWin bs (t,s)
    | otherwise   = slowestWin bs (worst_t, worst_s)

part1 :: [Board] -> [Int] -> Int
part1 bs ns = score
    where
        (_, score) = quickestWin ([playBoard b ns 1 | b <- bs]) (0,0)

part2 :: [Board] -> [Int] -> Int 
part2 bs ns = score
    where
        (_, score) = slowestWin ([playBoard b ns 1 | b <- bs]) (0, 0)

main :: IO()
main = do
    file <- readFile "data/day4.data"
    putStr "(Part 1, Part 2): "
    let
        (bs, ns) = parseInput file
        in do
        putStr "\t"
        print (part1 bs ns)
        putStr "\t"
        print (part2 bs ns)
