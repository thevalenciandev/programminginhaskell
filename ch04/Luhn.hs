luhnDouble :: Int -> Int
luhnDouble x
  | x > 9     = double x - 9
  | otherwise = double x

double :: Int -> Int
double x = x + x

luhn :: Int -> Int -> Int -> Int -> Bool
luhn x y z k = (luhnDouble x + double y + luhnDouble z + double k) `mod` 10 == 0
