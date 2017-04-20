module Koan.Ord where

import           Prelude hiding (max, maximum, min, minimum)

enrolled :: Bool
enrolled = False

-- Introduction to generics

max :: Ord a => a -> a -> a
max = undefined

min :: Ord a => a -> a -> a
min = undefined

maximum :: Ord a => [a] -> a
maximum = undefined

minimum :: Ord a => [a] -> a
minimum = undefined

sort :: Ord a => [a] -> [a]
sort = undefined
