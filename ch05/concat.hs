-- Flatten a list of lists into just a list
-- This shows how you can use the result of one generator
-- into another one
concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]
