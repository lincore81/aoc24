{-
 - Task:
 - Take the puzzle input is a list of integers.
 - Iterate 25x (part 1) or 75x (part 2) over this list, feeding output of
   iteration(i) into input of iteration(i+1), and apply the following rules to
   each number:
    - 0 -> 1
    - n | has even #digits -> [p, q] where show n == p ++ q and length p == length q
    - n -> n * 2024
 - Return the amount of numbers after the final iteration
 -}

input :: [Int]
input = [5688, 62084, 2, 3248809, 179, 79, 0, 172169]

splitDigits :: Int -> [Int]
splitDigits n | n < 10 = [0, n]
splitDigits n = [lhs, rhs]
 where
  str = show n
  halfLen = length str `div` 2
  lhs = read $ take halfLen str
  rhs = read $ drop halfLen str

evenDigits :: Int -> Bool
evenDigits = even . length . show

applyRules :: Int -> [Int]
applyRules 0 = [1]
applyRules n | evenDigits n = splitDigits n
applyRules n = [n * 2024]

-- This solution works for 25 iterations (~2secs on my machine), but not for 75
-- due to its inefficieny:
-- - All iterations are stored in memory
-- - Each iteration is flattened into a single list
solveNaive :: Int -> [Int] -> Int
solveNaive n = length . last . take (n + 1) . iterate (concatMap applyRules)

-- Better solution that ignores order and uses a HashMap to store partial results.
solve :: Int -> [Int] -> Int
solve = undefined

main :: IO ()
main = print (part1) -- , part2)
 where
  part1 = solveNaive 25 input
