module Koan.Eq where

import           Prelude hiding (elem, filter)

enrolled :: Bool
enrolled = False

-- Introduction to generics

filterInt :: (Int -> Bool) -> [Int] -> [Int]
filterInt = undefined

filterChar :: (Char -> Bool) -> [Char] -> [Char]
filterChar = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

-- Using the Eq typeclass

elemInt :: Int -> [Int] -> Bool
elemInt = undefined

elem :: Eq a => a -> [a] -> Bool
elem = undefined
