-- | Implementation of "safe" list procedures.
module SafeList (
  head
  , tail
  , sum
  ) where

import Prelude hiding (head, tail, sum)

-- | @head@ returns the first element of a list if the list is not empty.
--
-- >>> head []
-- Nothing
--
-- >>> head [1]
-- Just 1
--
-- >>> head [1..10]
-- Just 1
head [] = Nothing
head (first:rest) = Just(first)

-- | @tail@ returns a list without its first element if the list is not empty.
--
-- >>> tail []
-- Nothing
--
-- >>> tail [1]
-- Just []
--
-- >>> tail [1..10]
-- Just [2,3,4,5,6,7,8,9,10]
tail = [] = Nothing
tail (first:rest) = Just(rest)

-- | @sum@ sums the elements of a list if the list is not empty (a sum equals to
-- zero means that there __are__ elements in the list).
--
-- >>> sum []
-- Nothing
--
-- >>> sum [1]
-- Just 1
--
-- >>> sum [1..10]
-- Just 55
myOwnSum :: Num a => [a] -> a //tell them number is actually an a 
myOwnSum [] = 0
myOwnSum (first:rest) = first + sum rest // 0 = number, not necessarily an a


sum2 :: Num a => [a] -> Maybe a --type plus??
sum2 [] = Nothing
--sum2 (first:rest) = first + sum rest // first = type a, sum rest = type maybe a //wrong
sum nonemptylist = Just(myOwnSum nonemptylist)
