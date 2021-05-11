import Data.Char
import System.IO

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getNumber
           ns <- getNumbers' n
           printTotal (sum ns)
           return ()
           

getNumbers :: Int -> [Int] -> IO ()
getNumbers n xs = do if n==0 then
                        do printTotal (sum xs)
                           return ()
                     else 
                        do putChar '\n'
                           num <- getNumber
                           getNumbers (n-1) (num:xs)

getNumbers' :: Int -> IO [Int]
getNumbers' n = sequence [getNumber | _ <- [1..n]]

getNumber :: IO Int
getNumber = do n <- readLine
               return (read n :: Int)

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

readLine :: IO String
readLine = readLine' []
readLine' xs = do x <- getCh
                  if x == '\n' then
                    do putChar x
                       return xs
                  else if x == '\DEL' then
                    do putStr "\b \b"
                       readLine' (init xs)
                  else
                    do putChar x
                       readLine' (xs ++ [x])
            
getDigit :: IO Int
getDigit = do n <- getChar
              if isDigit n then
                return (digitToInt n)
              else
                do putStrLn "\nERROR: Invalid digit"
                   getDigit

printTotal :: Int -> IO ()
printTotal t = putStrLn ("The total is " ++ (show t))
