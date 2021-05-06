-- Alternative way of defining mult, by being
-- explicit with how currying works in practice
-- by using lambda functions
mult' :: Int -> (Int -> (Int -> Int))
mult' = \x -> (\y -> (\z -> x * y * z))
