module Main where

import Data.Text (pack, splitOn, unpack)

{-
 Task:
 - Read input and split content into two parts at the blank line
 - The first part is a list of order rules in the shape
   "a|b", where a and b are integers. The rule is that a must occur before b.
 - The second part is a line-separated list of comma-separated numeric sequences.
 - For each sequence, if all order rules apply, the sequence is valid.
 - The middle value of each valid sequence should be added up and printed.
   All sequences seem to be of odd length.

 Challenges:
 - Seems trivial to implement, but avoiding exponential complexity requires some thought.
-}

newtype PageOrder = Order (Int, Int)

loadInput :: String -> IO ([[Int]], [[Int]])
loadInput filename = do
  input <- readFile filename
  let parts = splitOn (pack "\n\n") (pack input)
  let [rules, sequences] = lines . unpack <$> parts
  let rules' = fmap (read . unpack) . splitOn (pack "|") . pack <$> rules
  let sequences' = fmap (read . unpack) . splitOn (pack ",") . pack <$> sequences
  return (rules', sequences')

main :: IO ()
main = loadInput "./input.txt" >>= print
