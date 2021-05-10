-- Clear screen
cls :: IO ()
cls = putStr "\ESC[2J"

-- Each character on the screen is given by
-- a par (x,y) of +ve integers with (1,1) being
-- the top-left corner
type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

