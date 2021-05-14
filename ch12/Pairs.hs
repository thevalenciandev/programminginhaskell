pairs :: Monad m => m a -> m b -> m (a,b)
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)
