-- Clear screen
cls :: IO ()
cls = putStr "\ESC[2J"

-- Each character on the screen is given by
-- a par (x,y) of +ve integers with (1,1) being
-- the top-left corner
type Pos = (Int, Int)
type Board = [Pos]

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "0" | p <- b]

width :: Int
width = 10

height :: Int
height = 10

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p) 

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,  y-1),
                          (x+1,y-1), (x-1,y),
                          (x+1,y),   (x-1,y+1),
                          (x,y+1),   (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)
