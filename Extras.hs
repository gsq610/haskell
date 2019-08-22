{-# LANGUAGE NoImplicitPrelude #-}

module Extras where

import Base
import Functions

-- | Apply a function to a list then flatten the results.
--
-- /Note/: in Haskell, @flatMap@ is called @concatMap@.
--
-- >>> flatMap (\x -> [x, x + 1, x + 2]) [1, 2, 3]
-- [1,2,3,2,3,4,3,4,5]
--
-- >>> flatMap (\x -> [sum x, product x]) [[1], [1, 2], [1, 2, 3]]
-- [1,1,3,2,6,6]
flatMap :: (a1 -> [a]) -> [a1] -> [a]
flatMap f = flatten . map f

-- | Rewrite @flatten@ using @flatMap@.
flatten' :: (a1 -> [a]) -> [a1] -> [a]
flatten' = flatMap

-- | Like a fold but accumulate the successive results.
--
-- /Note/: last (scanl f z xs) == foldl f z xs
-- >>> import GHC.Real
-- >>> import GHC.Classes as C
--
-- >>> scanl (/) 64 [4, 2, 4]
-- [64.0,16.0,8.0,2.0]
--
-- >>> scanl C.max 5 [1..10]
-- [5,5,5,5,5,5,6,7,8,9,10]
--
-- QuickCheck cannot deal with functions.
-- 
-- - prop> last (scanl (*) z xs) == foldl (*) z xs
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl _ q [] = [q]
scanl f q (x: xs) = q: scanl f (f q x) xs

-- | Return the elements of a list until not matching a predicate.
--
-- /Hint/: This is @takeUntil@ from your previous assignment.
-- >>> import GHC.Classes
-- >>> takeWhile (<= 5) [1..10]
-- [1,2,3,4,5]
--
-- >>> takeWhile (> 5) [1..10]
-- []
takeWhile :: (t -> Bool) -> [t] -> [t]
takeWhile _ [] = []
takeWhile p (x: xs)
  | p x = x : takeWhile p xs
  | otherwise = []
