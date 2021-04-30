qsort' [] = []
qsort' (x:xs) = qsort' ys ++ [x] ++ qsort' zs
                where
                  ys = [y | y<-xs, y<=x]
                  zs = [z | z<-xs, z>x]
