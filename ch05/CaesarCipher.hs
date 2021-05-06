import Data.Char

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr (n + ord 'a')

-- Shift lower-case letters by n positions, leaving
-- upper-case ones unchanged, for simplicity
shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | otherwise = c

-- If used with a positive n, this encodes a String
-- If used with a negative n, this can decode an encoded String
encode :: Int -> String -> String
encode n xs = [shift n x | x<-xs]
