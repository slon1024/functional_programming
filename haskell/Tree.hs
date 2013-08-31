module Tree where

data Tree a = Empty | Node (Tree a) a (Tree a)

insert :: (Ord a) => a -> Tree a -> Tree a
insert e Empty = Node Empty e Empty
insert e (Node t0 n t1) | e < n  = Node (insert e t0) n t1
                        | e >= n = Node t0 n (insert e t1)

buildTree :: (Ord a) => [a] -> Tree a
buildTree = foldr insert Empty

foldTree :: (a -> b -> b) -> b -> Tree a -> b
foldTree _ seed Empty          = seed
foldTree f seed (Node t0 n t1) =  foldTree f (f n (foldTree f seed t1)) t0 

flattenTree :: Tree a -> [a]
flattenTree = foldTree (:) []
