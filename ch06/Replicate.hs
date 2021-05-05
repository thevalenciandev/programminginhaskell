replicate' :: Int -> a -> [a]
replicate' n x 
  | n > 0     = x:replicate' (n-1) x
  | otherwise = []
