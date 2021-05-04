-- Scalar product of two lists of integers xs and ys
-- of length n is given by the sum of the products of
-- the corresponding integers
-- Using indexing
sp1 :: [Int] -> [Int] -> Int
sp1 xs ys = sum [xs!!i * ys!!i | i<-[0..n-1]]
            where n = length xs

-- Alternative version, with zip
sp2 :: [Int] -> [Int] -> Int
sp2 xs ys = sum [x*y | (x,y)<-zip xs ys]
