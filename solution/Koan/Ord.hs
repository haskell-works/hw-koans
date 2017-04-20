module Koan.Ord where

import           Prelude hiding (max, maximum, min, minimum)

enrolled :: Bool
enrolled = True

-- Introduction to generics

max :: Ord a => a -> a -> a
max a b = if a > b then a else b

min :: Ord a => a -> a -> a
min a b = if a < b then a else b

maximum :: Ord a => [a] -> a
maximum (x:y:xs) = x `max` maximum (y:xs)
maximum [x]      = x

minimum :: Ord a => [a] -> a
minimum (x:y:xs) = x `min` minimum (y:xs)
minimum [x]      = x

insert :: Ord a => a -> [a] -> [a]
insert e ls = go (compare) e ls
  where go _   x []         = [x]
        go cmp x ys@(y:ys') = case cmp x y of
            GT -> y : go cmp x ys'
            _  -> x : ys

sort :: Ord a => [a] -> [a]
sort (pivot:xs) = sort [x | x <- xs, x < pivot] ++ [pivot] ++ sort [x | x <- xs, x >= pivot]
sort []         = []
