-- Commands -> Height -> Depth -> Returns: Height * Depth 
part1 :: [String] -> Int -> Int -> Int
part1 [] h d = h * d
part1 (c:cs) h d 
        | cmd == "forward" = part1 cs (h + x) d
        | cmd == "down"    = part1 cs h (d + x)
        | cmd == "up"      = part1 cs h (d - x)
        | otherwise        = part1 cs h d -- Skip if unrecognised
    where
        cmd = head (words c)
        x   = read (head (tail (words c)))

-- Commands -> Aim -> Height -> Depth -> Returns: Height * Depth 
part2 :: [String] -> Int -> Int -> Int -> Int
part2 [] _ h d = h * d
part2 (c:cs) a h d 
        | cmd == "down"    = part2 cs (a + x) h d
        | cmd == "up"      = part2 cs (a - x) h d
        | cmd == "forward" = part2 cs a (h + x) (d + (a * x))
        | otherwise        = part2 cs a h d -- Skip if unrecognised
    where
        cmd = head (words c)
        x   = read (head (tail (words c)))

main :: IO()
main = do
    input <- readFile "data.txt"
    putStr "Part 1 (height, depth): "
    print (part1 (lines input) 0 0)
    putStr "Part 2: "
    print (part2 (lines input) 0 0 0)