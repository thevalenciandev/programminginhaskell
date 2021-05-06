-- Returns a list of all values that are associated
-- with a given key in a lookup table
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']
