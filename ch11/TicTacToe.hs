import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Grid = [[Player]]

-- Ordering on constructors is determined by their position
-- in the data declaration, ie. 0 < B < X
-- B means Blank
data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B -- For completeness
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full g = (all (/=B) (concat g))

-- turn empty = O. O will be the human player
turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
            os = length (filter (==O) ps)
            xs = length (filter (==X) ps)
            ps = concat g

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              line = all (== p)
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

putGrid :: Grid -> IO ()
putGrid =
  putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
              beside = foldr1 (zipWith (++))
              bar    = replicate 3 "|" 

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

