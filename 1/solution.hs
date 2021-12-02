-- Count the number of times the value increases compared to the previous
-- input data -> previous value -> count
part1 :: [Int] -> Int -> Int -> Int
part1 [] _ cnt = cnt
part1 (r:rs) 0 0 = part1 rs r 0 -- Initial case
part1 (r:rs) prev cnt
    | r > prev  = part1 rs r cnt+1
    | otherwise = part1 rs r cnt

part1zip :: [Int] -> Int
part1zip []   = 0
part1zip nums = length (filter id (zipWith (>) (tail nums) nums))

-- Count the number of times the three-value sliding window increases
-- input data -> previous window value -> count
part2 :: [Int] -> Int -> Int -> Int
part2 [r1, r2] _ cnt = cnt -- Break when we cannot create a three value window
part2 (r1:r2:r3:rs) 0 0 = part2 (r2:r3:rs) (r1 + r2 + r3) 0 -- Initial case
part2 (r1:r2:r3:rs) prev cnt
    | r1 + r2 + r3 > prev = part2 (r2:r3:rs) (r1 + r2 + r3) (cnt + 1)
    | otherwise           = part2 (r2:r3:rs) (r1 + r2 + r3) cnt
part2 _ _ _ = -1 -- This should never be hit

part2zip :: [Int] -> Int
part2zip []   = 0
part2zip nums = part1zip (zipWith3 (\a b c -> a + b + c) nums (tail nums) (tail (tail nums)))

-- Helper function to read
parseFile :: String -> [Int]
parseFile file = map read (lines file)

main :: IO()
main = do
    input <- readFile "data.txt"
    putStr "Part 1: "
    print (part1zip (parseFile input))
    putStr "Part 2: "
    print (part2zip (parseFile input))
