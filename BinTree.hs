module BinTree where

data BinTree = Nil | Node Int BinTree BinTree
  deriving (Show)

-- $setup
-- >>> tree = Node 16 (Node 23 Nil (Node 73 Nil Nil)) (Node 42 Nil Nil)
-- >>> one = Node 1 Nil Nil

-- | Find the depth of a tree (number of levels)
--
-- >>> depth Nil
-- 0
--
-- >>> depth (Node 1 Nil Nil)
-- 1
--
-- >>> depth tree
-- 3
depth :: BinTree -> Int 
depth Nil = 0
depth (Node v l r) = 1 + max (depth l) (depth r)

-- | Find the number of nodes in a tree.
--
-- >>> size Nil
-- 0
--
-- >>> size one
-- 1
--
-- >>> size tree
-- 4
size :: BinTree -> Int
size Nil = 0
size (Node v l r) = 1 + size l + size r

-- | Sum the elements of a numeric tree.
--
-- >>> sumTree Nil
-- 0
--
-- >>> sumTree one
-- 1
--
-- >>> sumTree tree
-- 154
--
-- prop> sumTree (Node v Nil Nil) == v
sumTree :: BinTree -> Int
sumTree Nil = 0
sumTree (Node v l r) = v + sumTree l + sumTree r

-- | Find the minimum element in a tree.
-- | e.g. minTree <your pattern here> = error "Tree is empty"
--
-- >>> minTree one
-- 1
--
-- >>> minTree tree
-- 16
--
minTree :: BinTree -> Int
minTree Nil = error "Tree is empty"
minTree (Node x Nil Nil) = x
minTree (Node x l Nil) = min x $ minTree l
minTree (Node x Nil r) = min x $ minTree r
minTree (Node x l r) = min x $ min (minTree l) (minTree r) 

-- | Map a function over a tree.
--
-- >>> mapTree (+1) Nil
-- Nil
--
-- >>> mapTree (*1) one
-- Node 1 Nil Nil
--
-- >>> mapTree ((flip mod) 2) tree
-- Node 0 (Node 1 Nil (Node 1 Nil Nil)) (Node 0 Nil Nil)
mapTree :: (Int -> Int) -> BinTree -> BinTree
mapTree _ Nil = Nil
mapTree f (Node v l r) = Node (f v) (mapTree f l) (mapTree f r)
