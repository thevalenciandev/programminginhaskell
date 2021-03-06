-- Parse is just a synonym of a fn that takes a String
-- and returns a list of results, each of which is a pair
-- comprising a result value of type a and an output String
-- If failure, then it'll be an empty list, else it'll be
-- a singleton list.
-- type Parser a = String -> [(a,String)]

-- We use newtype to make a completely new type
-- with a single constructor P, so we can use it
-- to implement a type class later
newtype Parser a = P (String -> [(a,String)])

-- Here we define the fn parse which deconstructs
-- the parser to get rid of the dummy constructor P
-- and just applies the fn p to the input inp
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

-- Definition of a parsing primitive: item
item :: Parser Char
item = P (\inp -> case inp of
                    []     -> []
                    (x:xs) -> [(x,xs)])

-- Implementing Functor, Applicative and Monad
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> [(g v, out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> [(v,inp)])
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\inp -> case parse pg inp of
                          []        -> []
                          [(g,out)] -> parse (fmap g px) out)

three :: Parser (Char,Char)
three = pure g <*> item <*> item <*> item
        where g x y z = (x,z)
