elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (x':xs)
  | x == x'   = True
  | otherwise = elem' x xs
