-- Removes last element
init' :: [a] -> [a]
init' xs = reverse $ drop 1 $ reverse xs 

-- Another way of doing it
init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs
