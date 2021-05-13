data Expr = Val Int | Div Expr Expr
            deriving Show

eval :: Expr -> Int
eval (Val x)   = x
eval (Div x y) = eval x `div` eval y

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv x y = Just (x `div` y)

safeeval :: Expr -> Maybe Int
safeeval (Val x)   = Just x
safeeval (Div x y) = case safeeval x of
                        Nothing -> Nothing
                        Just n  -> case safeeval y of
                                      Nothing -> Nothing
                                      Just m  -> safediv n m
