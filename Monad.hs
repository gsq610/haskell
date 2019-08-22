{-# LANGUAGE NoImplicitPrelude #-}


module Monad where

import Base
import Functions
import Functor
import Applicative hiding((<*>), (<$>))

-- | All instances of the @Monad@ type-class must satisfy one law. This law
-- is not checked by the compiler. This law is given as:
--
-- * The law of associativity
--   g =<< (f =<< x) = ((g =<<) . f) =<< x
class Applicative f => Monad f where
  -- Pronounced: "bind."
  (=<<) :: (a -> f b) -> f a -> f b

infixr 1 =<<

-- | Rewrite /apply/ @(<*>)@ using /bind/ @(=<<)@ and /fmap/ @(<$>)@.
(<*>) :: (Monad f) => f (a -> b) -> f a -> f b
(<*>) f x = (<$> x) =<< f

infixl 4 <*>

-- | Bind a function on a list.
--
-- >>> (\n -> [n, n]) =<< [1, 2, 3]
-- [1,1,2,2,3,3]
instance Monad [] where
  (=<<) _ [] = []
  (=<<) f (x: xs) = (f x) ++ (f =<< xs)

-- | Bind a function on a @Maybe@.
--
-- >>> justDouble n = Just (n + n)
-- >>> justDouble =<< Just 7
-- Just 14
--
-- >>> justDouble =<< Nothing
-- Nothing
instance Monad Maybe where
  (=<<) _ Nothing = Nothing
  (=<<) f (Just x) = f x

-- | Flatten a combined structure to a single structure.
--
-- >>> join [[1, 2, 3], [1, 2]]
-- [1,2,3,1,2]
--
-- >>> join (Just Nothing)
-- Nothing
--
-- >>> join (Just (Just 7))
-- Just 7
join :: (Monad m) => m (m a) -> m a
join = (id =<<)

-- | Implement a flipped version of @(=<<)@, however, use only @join@ and
-- @(<$>)@.
--
-- >>> :{
-- half x | even x    = Just (x `div` 2)
--        | otherwise = Nothing
-- :}
--
-- >>> Just 20 >>= half
-- Just 10
--
-- >>> Just 20 >>= half >>= half >>= half
-- Nothing
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
(>>=) x f = join (f <$> x)

-- | Implement composition within the @Monad@ environment. Called: "Kleisli
-- composition."
--
-- >>> (\n -> [n, n]) <=< (\n -> [n + 1, n + 2]) $ 1
-- [2,2,3,3]
(<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> a -> m c
(<=<) f g = (=<<) f . g

infixr 1 <=<

-- | Fold a list using a monadic function.
--
-- /Tip:/ Fold the monad from left to right on the list of arguments.
--
-- Sum the elements in a list, but fail if any is greater than 10.
-- >>> :{
-- small acc x
--   | x < 10 = Just (acc + x)
--   | otherwise = Nothing
-- :}
--
-- >>> foldM small 0 [1..9]
-- Just 45
--
-- >>> foldM small 0 [1..100]
-- Nothing
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM _ y [] = pure y           -- return
foldM f y (x: xs) = (`f` x) =<< foldM f y xs
