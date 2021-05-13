data Expr = Val Int | Div Expr Expr
            deriving Show

eval :: Expr -> Int
eval (Val x)   = x
eval (Div x y) = eval x `div` eval y
