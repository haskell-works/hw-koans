{-# LANGUAGE NoImplicitPrelude #-}

module Koan.Simple where

import Prelude (Bool (..), Eq (..), Int, Maybe (..), Num (..), Ord ((<), (<=), (>), (>=)))
import Prelude (otherwise, tail, (&&), (||))

enrolled :: Bool
enrolled = False

id :: a -> a
id a = a

const :: a -> b -> a
const a _ = a

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

($) :: (a -> b) -> a -> b
f $ a = f a

infixr 0 $

--------------------------------------------------------------------------------
-- LISTS
--------------------------------------------------------------------------------

length :: [a] -> Int
length = foldr (\_ b -> b + 1) 0

(!!) :: [a] -> Int -> Maybe a
[] !! _ = Nothing
(x:xs) !! i
  | i < 0     = Nothing
  | i == 0    = Just x
  | otherwise = xs !! (i - 1)

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x:(xs ++ ys)

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

--------------------------------------------------------------------------------
-- Infinite lists
--------------------------------------------------------------------------------

repeat :: a -> [a]
repeat x = x : repeat x

iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)

take :: Int -> [a] -> [a]
take n (x:xs)
  | n <= 0    = []
  | otherwise = x : take (n - 1) xs

drop :: Int -> [a] -> [a]
drop n xs     | n <= 0 = xs
drop n (x:xs) = drop (n - 1) xs
drop _ []     = []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p (x:xs) | p x = x : takeWhile p xs
takeWhile _ _      = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []  = []
dropWhile p (x:xs)
  | p x       = dropWhile p xs
  | otherwise = x:xs

--------------------------------------------------------------------------------
-- Higher order functions on lists
--------------------------------------------------------------------------------

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl _  acc []     = acc
foldl op acc (x:xs) =
  let acc' = op acc x
  in foldl op acc' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _  acc []     = acc
foldr op acc (x:xs) = op x (foldr op acc xs)

any :: (a -> Bool) -> [a] -> Bool
any p = foldl (\b a -> b || p a) False

all :: (a -> Bool) -> [a] -> Bool
all p = foldl (\b a -> b && p a) True

zip :: [a] -> [b] -> [(a, b)]
zip [] _          = []
zip _ []          = []
zip (x:xs) (y:ys) = (x, y):zip xs ys

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _           = []
zipWith _ _ []           = []
zipWith op (x:xs) (y:ys) = op x y : zipWith op xs ys

--------------------------------------------------------------------------------
-- Currying
--------------------------------------------------------------------------------

curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a,b)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a,b) = f a b

--------------------------------------------------------------------------------
-- Functions require Equality
--------------------------------------------------------------------------------

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem e (x:xs)
  | e == x    = True
  | otherwise = e `elem` xs

group :: Eq a => [a] -> [[a]]
group (x:xs) = case slurp x xs of
  (ys, zs) -> (x:ys):group zs
  where slurp :: Eq a => a -> [a] -> ([a], [a])
        slurp a (b:bs) | a == b = (b:cs, ds)
          where (cs, ds) = slurp a bs
        slurp _ bs = ([], bs)
group [] = []

isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf (a:as) (b:bs) = if a == b then isPrefixOf as bs else False
isPrefixOf [] bs         = True
isPrefixOf _ _           = False

isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf as bs = isPrefixOf (reverse as) (reverse bs)

isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf as bs = if as `isPrefixOf` bs
  then True
  else case bs of
    (c:cs) -> as `isInfixOf` cs
    []     -> False

isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf aas@(a:as) bss@(b:bs) = (a == b && isSubsequenceOf as bs) || isSubsequenceOf aas bs
isSubsequenceOf [] _                  = True
isSubsequenceOf _ []                  = False

--------------------------------------------------------------------------------
-- Functions require Ordering
--------------------------------------------------------------------------------

max :: Ord a => a -> a -> a
max x y = if x > y then x else y

min :: Ord a => a -> a -> a
min x y = if x < y then x else y

maximum :: Ord a => [a] -> a
maximum (x:xs) = foldr max x xs

minimum :: Ord a => [a] -> a
minimum (x:xs) = foldr min x xs

sort :: Ord a => [a] -> [a]
sort (a:as) = sort (filter (< a) as) ++ [a] ++ sort (filter (>= a) as)
sort []     = []

--------------------------------------------------------------------------------
-- Miscellaneous Exercises
--------------------------------------------------------------------------------

fibonacci :: [Int]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)
