module Pair where

-- | A pair has two elements of the same type.
data Pair = Pair Int Int
  deriving(Show)

-- $setup
-- >>> p1 = (Pair 5 6)
-- >>> p2 = (Pair 7 1)
-- >>> p3 = (Pair 9 9)

-- | Sum the two element of a pair.
--
-- >>> plusPair p1
-- 11
--
-- >>> plusPair p2
-- 8
plusPair :: Pair -> Int
plusPair (Pair x y) = x + y

-- | Subtract the two elements of a pair.
--
-- >>> minusPair p1
-- -1
--
-- >>> minusPair p2
-- 6
minusPair :: Pair -> Int
minusPair (Pair x y) = x - y

-- | Return the maximum element in a pair.
--
-- >>> maxPair p1
-- 6
--
-- >>> maxPair p2
-- 7
maxPair :: Pair -> Int
maxPair (Pair x y) = if x > y then x else y

-- | Add two pairs together.
--
-- >>> addPair p1 p2
-- Pair 12 7
addPair :: Pair -> Pair -> Pair
addPair (Pair a b) (Pair c d)= Pair (a+c) (b+d)

-- | Subtract two pairs together.
--
-- >>> subPair p1 p2
-- Pair (-2) 5
subPair :: Pair -> Pair -> Pair
subPair (Pair a b) (Pair c d)= Pair (a-c) (b-d)
