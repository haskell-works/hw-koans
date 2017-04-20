module Koan.Eq where

import           Prelude hiding (elem, filter)

enrolled :: Bool
enrolled = False

-- Introduction to generics

filterInt :: (Int -> Bool) -> [Int] -> [Int]
filterInt = error "Implement filterInt"

filterChar :: (Char -> Bool) -> [Char] -> [Char]
filterChar = error "Implement filterChar"

filter :: (a -> Bool) -> [a] -> [a]
filter = error "Implement filter"

-- Using the Eq typeclass

elemInt :: Int -> [Int] -> Bool
elemInt = error "Implement elemInt"

elem :: Eq a => a -> [a] -> Bool
elem = error "Implement elem"

nub :: Eq a => [a] -> [a]
nub = error "Implement nub"

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf = error "Implement isPrefixOf"
