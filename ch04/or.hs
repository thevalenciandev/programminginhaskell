or1 :: Bool -> Bool -> Bool
or1 True _  = True
or1 False y = y

or2 :: Bool -> Bool -> Bool
or2 x y = if x then True
          else if y then True
               else False
