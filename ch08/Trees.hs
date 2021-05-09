data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

-- Preorder search (leaf, left subtree then right subtree)
occurs :: Eq a => a -> Tree a -> Bool
occurs v (Leaf v')     = v == v'
occurs v (Node l v' r) = v == v' || occurs v l || occurs v r 

-- If we flatten a tree and it returns an ordered list,
-- then this tree is a search tree
flatten :: Tree a -> [a]
flatten (Leaf v)      = [v]
flatten (Node l v' r) = flatten l ++ [v'] ++ flatten r

-- Optimised version of occurs for BST
occursbst :: Ord a => a -> Tree a -> Bool
occursbst v (Leaf v')     = v == v'
occursbst v (Node l v' r) | v < v'    = occursbst v l
                          | v > v'    = occursbst v r
                          | otherwise = True 
