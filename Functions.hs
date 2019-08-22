{-# LANGUAGE NoImplicitPrelude #-}
-- | The goal of this module is to rewrite "standard" Haskell functions using
-- `fold` only.
module Functions where

import Base

-- $setup
-- import Test.QuickCheck
-- import Prelude(foldl)

-- | Rewrite @sum@ using @foldr@.
--
-- /Hint/: Consider everything to be an @Integer@.
--
-- >>> sum [1, 2, 3]
-- 6
--
-- >>> sum [1..10]
-- 55
--
-- prop> \x -> foldl (-) (sum x) x == 0
sum :: (Num a) => [a] -> a
sum = foldr (+) 0

-- | Rewrite @product@ using @foldr@.
--
-- /Hint/: Consider everything to be an @Integer@.
--
-- >>> product [1, 2, 3]
-- 6
--
-- >>> product [1..10]
-- 3628800
product :: (Num a) => [a] -> a
product = foldr (*) 1

-- | Rewrite @length@ using @foldr@.
--
-- /Hint/: Return an @Integer@.
--
-- >>> length [1, 2, 3]
-- 3
--
-- >>> length []
-- 0
--
-- prop> sum (map (const 1) x) == length x
length :: [a] -> Int
length = foldr (\_ acc -> acc + 1) 0

-- | Rewrite @map@ using @foldr@.
--
-- >>> map (+ 1) [1, 2, 3]
-- [2,3,4]
--
-- >>> map (* 2) [1, 2, 3]
-- [2,4,6]
--
-- prop> map id x == x
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x xs -> f x : xs) []

-- | Rewrite @filter@ using @foldr@.
--
-- >>> filter (< 2) [1, 2, 3]
-- [1]
--
-- >>> filter even [1, 2, 3, 4, 5]
-- [2,4]
--
-- prop> filter (const True) l == l
--
-- prop> filter (const False) l == []
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x -> if p x then (x:) else id) []

-- | Rewrite /append/ @(++)@ using @foldr@.
--
-- /Optional/: write this in point-free notation
--
-- >>> [1] ++ [2] ++ [3]
-- [1,2,3]
--
-- >>> "abc" ++ "d"
-- "abcd"
--
-- prop> (x ++ []) == x
--
-- Associativity of append.
-- prop> (x ++ y) ++ z == x ++ (y ++ z)
(++) :: [a] -> [a] -> [a]
(++) = flip (foldr (:))

-- | Rewrite @all@ using @foldr@.
--
-- >>> all [True, True, True]
-- True
--
-- >>> all [False, True, True]
-- False
all :: [Bool] -> Bool
all = foldr (&&) True

-- | Rewrite @any@ using @foldr@.
--
-- >>> any [False, False, False]
-- False
--
-- >>> any [False, True, False]
-- True
any :: [Bool] -> Bool
any = foldr (||) False

-- | Flatten a (once) nested list.
--
-- >>> flatten [[1], [2], [3]]
-- [1,2,3]
--
-- >>> flatten [[1, 2], [3], []]
-- [1,2,3]
--
-- prop> sum (map length x) == length (flatten x)
flatten :: [[a]] -> [a]
flatten = foldr (++) []
-- flatten' = flatMap . id

-- Optionals, you may use other functions

-- | Return the first element of a list or a default.
--
-- /Note/: You __have to__ use @foldr@.
--
-- >>> headOr 3 [1, 2]
-- 1
--
-- >>> headOr 3 []
-- 3
--
-- prop> \x -> x `headOr` [0..] == 0
--
-- prop> x `headOr` [] == x
headOr :: a -> [a] -> a
headOr = foldr const

-- | Find the first element in a list matching a predicate.
--
-- >>> find even [1, 2, 3, 4]
-- Just 2
--
-- >>> find even [1, 3, 5]
-- Nothing
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find p (x: xs)
  | p x = Just x
  | otherwise = find p xs
-- find p x = case filter p x of
--   [] -> Nothing
--   h: _ -> Just h
