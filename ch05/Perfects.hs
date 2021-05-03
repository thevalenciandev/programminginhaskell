module Perfects where

import Factors
-- A positive integer is perfect if it equals the sum
-- of all its factors, excluding the number itself.
-- The below fn calculates all the perfect numbers up
-- to a given limit
perfects :: Int -> [Int]
perfects n = [x | x<-[1..n], x == (sum $ init $ factors x)]
