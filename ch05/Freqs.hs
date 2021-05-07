-- Returns a frequency table of the letters
-- of the alphabet in a given String
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
            where n = length xs

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

count :: Eq a => a -> [a] -> Int
count x xs = length [x | x'<-xs, x == x']

-- Chi-square statistic function
chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [(o - e)^2 / e | (o, e) <- zip os es]

-- Rotate elements n places to the left
-- Eg. rotate 3 [1,2,3,4,5] => [4,5,1,2,3]
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs
