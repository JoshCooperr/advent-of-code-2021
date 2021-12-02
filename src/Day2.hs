module Day2 where

-- Commands, Height, Depth -> Returns: Height * Depth
part1 :: [Command] -> Int -> Int -> Int
part1 [] h d = h * d
part1 (c : cs) h d = case c of
    Forward x -> part1 cs (h + x) d
    Down x -> part1 cs h (d + x)
    Up x -> part1 cs h (d - x)

-- Commands, Aim, Height, Depth -> Returns: Height * Depth
part2 :: [String] -> Int -> Int -> Int -> Int
part2 [] _ h d = h * d
part2 (c : cs) a h d
    | cmd == "down" = part2 cs (a + read x) h d
    | cmd == "up" = part2 cs (a - read x) h d
    | cmd == "forward" = part2 cs a (h + read x) (d + (a * read x))
    | otherwise = part2 cs a h d -- Skip if unrecognised
    where
        [cmd, x] = words c

data Command = Forward Int | Down Int | Up Int

parse :: String -> Command
parse s = case words s of
    ["forward", x] -> Forward (read x)
    ["up", x] -> Up (read x)
    ["down", x] -> Down (read x)
    _ -> error "panic"

main :: IO ()
main = do
    input <- readFile "data/day2.data"
    putStr "Part 1: "
    print (part1 (map parse (lines input)) 0 0)
    putStr "Part 2: "
    print (part2 (lines input) 0 0 0)
