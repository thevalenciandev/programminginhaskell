module Factors where

-- Find all factors of a number n
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]
