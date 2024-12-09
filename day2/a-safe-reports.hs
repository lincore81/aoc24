module Main where

import Data.Text (pack, splitOn, unpack)

{-
 - https://adventofcode.com/2024/day/2
 - Task:
 -  1. Read a list of integer lists from a file, where each line contains a [Int].
 -  2. Count all lists that are safe sequences.
 -  3. A safe sequence is monotonic and differences between consecutive elements are between [1, 3].
 -  4. Print the count.
 -}

isMonotonic :: [Int] -> Bool
isMonotonic [] = True
isMonotonic [x] = True
isMonotonic (x : y : xs) = aux (signum $ x - y) (y : xs)
 where
  aux _ [] = True
  aux _ [x] = True
  aux sign (x : y : xs) = (signum (x - y) == sign) && aux sign (y : xs)

differenceBetween1And3 :: [Int] -> Bool
differenceBetween1And3 [] = True
differenceBetween1And3 [x] = True
differenceBetween1And3 (x : y : xs) = not (diff < 1 || diff > 3) && differenceBetween1And3 (y : xs)
 where
  diff = abs (x - y)

safeSeq :: [Int] -> Bool
safeSeq xs = all ($ xs) [differenceBetween1And3, isMonotonic]

loadInput :: String -> IO [[Int]]
loadInput filename = do
  contents <- readFile filename
  return $ map (map (read . unpack) . splitOn (pack " ") . pack) $ lines contents

solve :: String -> IO Int
solve filename = do
  input <- loadInput filename
  return $ length $ filter safeSeq input

main :: IO ()
main = solve "input.txt" >>= print
