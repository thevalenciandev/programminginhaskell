nth :: [a] -> Int -> a
nth [] _ = error "Index too large"
nth (x:xs) n
  | n < 0  = error "Negative index"
  | n == 0 = x
  | n > 0  = nth xs (n-1)
