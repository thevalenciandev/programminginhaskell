-- Gets the last element of a list
-- Throws if list is empty!
last' :: [a] -> a
last' xs = head $ reverse xs

-- Another way of doing it
-- Throws if list is empty!
last2 :: [a] -> a
last2 xs = xs !! (length xs - 1)

