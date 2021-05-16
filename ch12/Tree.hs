data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)   = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)

data Tree' a = Leaf' | Node' (Tree' a) a (Tree' a)
             deriving Show

instance Functor Tree' where
  -- fmap :: (a -> b) -> Tree' a -> Tree' b
  fmap g Leaf' = Leaf'
  fmap g (Node' l x r) = Node' (fmap g l) (g x) (fmap g r)
