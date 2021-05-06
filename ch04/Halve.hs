halve :: [a] -> ([a], [a])
halve [] = error "Cannot halve an empty list" 
halve xs
    | odd (length xs) = error "Cannot halve a list of odd elements"
    | otherwise       = (take half xs, drop half xs) 
                          where half = (length xs `div` 2)
