module Koan.Eq where

import           Prelude hiding (elem, filter)

enrolled :: Bool
enrolled = False

-- Introduction to generics

filterInt :: (Int -> Bool) -> [Int] -> [Int]
filterInt = error "TODO: implement filterInt"

filterChar :: (Char -> Bool) -> [Char] -> [Char]
filterChar = error "TODO: implement filterChar"

filter :: (a -> Bool) -> [a] -> [a]
filter = error "TODO: implement filter"

-- Using the Eq typeclass

elemInt :: Int -> [Int] -> Bool
elemInt = error "TODO: implement elemInt"

elem :: Eq a => a -> [a] -> Bool
elem = error "TODO: implement elem"

nub :: Eq a => [a] -> [a]
nub = error "TODO: implement nub"

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf = error "TODO: implement isPrefixOf"
