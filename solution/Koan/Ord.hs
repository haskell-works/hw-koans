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
