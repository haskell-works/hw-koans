module Koan.Eq where

import           Prelude hiding (elem, filter)

-- Introduction to generics

filterInt :: (Int -> Bool) -> [Int] -> [Int]
filterInt p (x:xs) = if p x then x:filterInt p xs else filterInt p xs
filterInt _ []     = []

filterChar :: (Char -> Bool) -> [Char] -> [Char]
filterChar p (x:xs) = if p x then x:filterChar p xs else filterChar p xs
filterChar _ []     = []

filter :: (a -> Bool) -> [a] -> [a]
filter p (x:xs) = if p x then x:filter p xs else filter p xs
filter _ []     = []

-- Using the Eq typeclass

elemInt :: Int -> [Int] -> Bool
elemInt i (x:xs) = if i == x then True else elemInt i xs
elemInt _ []     = False

elem :: Eq a => a -> [a] -> Bool
elem i (x:xs) = if i == x then True else elem i xs
elem _ []     = False
