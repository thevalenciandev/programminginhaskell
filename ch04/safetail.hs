-- Using a conditional expression
safetail1 :: [a] -> [a]
safetail1 xs = if null xs then xs else tail xs

-- Using guarded equations
safetail2 :: [a] -> [a]
safetail2 xs | null xs   = []
             | otherwise = tail xs

-- Using pattern matching
safetail3 :: [a] -> [a]
safetail3 []      = []
safetail3 (_:xs)  = xs
