module Koan.Ord where

import           Prelude hiding (max, maximum, min, minimum)

enrolled :: Bool
enrolled = False

-- | Get the greater of two values
max :: Ord a => a -> a -> a
max = undefined

-- | Get the lesser of two values
min :: Ord a => a -> a -> a
min = undefined

-- | Get the greatest element of the list.  The list must be finite and non-empty.
maximum :: Ord a => [a] -> a
maximum = undefined

-- | Get the least element of the list.  The list must be finite and non-empty.
minimum :: Ord a => [a] -> a
minimum = undefined

-- | The 'sort' function implements a stable sorting algorithm.
sort :: Ord a => [a] -> [a]
sort = undefined

-- | The 'insert' function takes an element and a list and inserts the
-- element into the list at the first position where it is less
-- than or equal to the next element.  In particular, if the list
-- is sorted before the call, the result will also be sorted.
-- It is a special case of 'insertBy', which allows the programmer to
-- supply their own comparison function.
insert :: Ord a => a -> [a] -> [a]
insert = undefined
