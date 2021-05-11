import Data.Char

adder :: IO ()
adder = do putStr "How many numbers? "
           n <- getNumber
           getNumbers n []

getNumbers :: Int -> [Int] -> IO ()
getNumbers n xs = do if n==0 then
                        do putStrLn ("\nThe total is " ++ (show (sum xs)))
                           return ()
                     else 
                        do putChar '\n'
                           num <- getNumber
                           getNumbers (n-1) (num:xs)
                

getNumber :: IO Int
getNumber = do n <- getLine
               return (read n :: Int)

getDigit :: IO Int
getDigit = do n <- getChar
              if isDigit n then
                return (digitToInt n)
              else
                do putStrLn "\nERROR: Invalid digit"
                   getDigit

