and' :: [Bool] -> Bool
and' []     = True
and' (x:xs)
  | not x     = False
  | otherwise = and' xs

