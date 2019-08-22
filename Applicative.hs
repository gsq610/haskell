{-# LANGUAGE InstanceSigs, NoImplicitPrelude #-}

module Applicative where

import Base
import Functor hiding ((<$>))
import Functions

-- | All instances of the `Applicative` type-class must satisfy three laws.
-- These laws are not checked by the compiler. These laws are given as:
--
-- * The law of associative composition
--   pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- * The law of left identity
--   pure id <*> v = v
--
-- * The law of right identity
--   v <*> pure id = v
class Functor f => Applicative f where
  pure :: a -> f a
  -- Pronounced "apply"
  (<*>) :: f (a -> b) -> f a -> f b

infixl 4 <*>

-- | The @Applicative@ class derives from @Functor@, rewrite `fmap` using only
-- `pure` and `<*>`.
--
-- >>> (+1) <$> (Id 2)
-- Id 3
--
-- >>> (+1) <$> Nothing
-- Nothing
--
-- >>> (+1) <$> [1, 2, 3]
-- [2,3,4]
(<$>) :: Applicative f => (a -> b) -> f a -> f b
(<$>) = (<*>) . pure
-- (<$>) f x = (pure f) <*> x

-- | Insert into Id.
--
-- >>> Id (+10) <*> Id 8
-- Id 18
instance Applicative Id where
  pure :: a -> Id a
  pure = Id

  (<*>) :: Id (a -> b) -> Id a -> Id b
  (<*>) (Id f) (Id x) = Id (f x)

-- | Apply a list of functions over a list of elements, producing the
-- concatenation of the successive results.
--
-- >>> [(+1), (*2)] <*> [1, 2, 3]
-- [2,3,4,2,4,6]
instance Applicative [] where
  pure :: a -> [a]
  pure x = x: [] -- [x]

  (<*>) :: [(a -> b)] -> [a] -> [b]
  (<*>) [] _ = []
  (<*>) _ [] = []
  (<*>) (f: fs) xs = (map f xs) ++ (fs <*> xs)

-- | Apply to a Maybe, must return `Nothing` if either the function or the
-- element is a `Nothing`.
--
-- >>> Just (+8) <*> Just 7
-- Just 15
--
-- >>> Nothing <*> Just 7
-- Nothing
--
-- >>> Just (+8) <*> Nothing
-- Nothing
instance Applicative Maybe where
  pure :: a -> Maybe a
  pure x = Just x

  (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
  (<*>) Nothing _ = Nothing
  (<*>) _ Nothing = Nothing
  (<*>) (Just f) (Just x) = Just (f x)

-- | Apply to a RoseTree, i.e. return a tree composed of trees created by the
-- successive application of functions to initial nodes.
--
-- >>> (Node (+1) []) <*> (Node 7 [Node 1 [], Node 2 [], Node 3 [Node 4 []]])
-- Node 8 [Node 2 [],Node 3 [],Node 4 [Node 5 []]]
--
-- >>> (Node (+1) [Node (*2) []]) <*> (Node 5 [Node 2 [], Node 8 [Node 1 []]])
-- Node 6 [Node 3 [],Node 9 [Node 2 []],Node 10 [Node 4 [],Node 16 [Node 2 []]]]
instance Applicative RoseTree where
  pure :: a -> RoseTree a
  pure x = Node x []

  (<*>) :: RoseTree (a -> b) -> RoseTree a -> RoseTree b
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Node f fs) node@(Node x xs) = Node (f x) ((mapF f xs) ++ (mapA node fs))
    where
      mapF = map . (<$>)
      mapA node = map (\f -> f <*> node)
