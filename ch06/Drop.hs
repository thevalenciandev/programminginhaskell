-- The below implementation does not work
-- if n < 0
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop (n-1) xs

-- Another impl. that does NOT drop anything
-- if n < 0
drop2 :: Int -> [a] -> [a]
drop2 _ [] = []
drop2 n xs@(x:xs')
  | n <= 0    = xs 
  | otherwise = drop2 (n-1) xs'
