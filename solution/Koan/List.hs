module Koan.List where

import Koan.Applicative as K
import Koan.Functor     as K
import Koan.Monad       as K
import Prelude          (Bool (..), Int, Num (..), flip, otherwise)

import qualified Prelude as P

enrolled :: Bool
enrolled = False

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

last :: [a] -> a
last [x]    = x
last (_:xs) = last xs

init :: [a] -> [a]
init [x]    = []
init (x:xs) = x:init xs

length :: [a] -> Int
length (_:xs) = 1 + length xs
length []     = 0

sumInt :: [Int] -> Int
sumInt (x:xs) = x + sumInt xs
sumInt []     = 0

productInt :: [Int] -> Int
productInt (x:xs) = x * productInt xs
productInt []     = 1

sum :: Num a => [a] -> a
sum (x:xs) = x + sum xs
sum []     = 0

product :: Num a => [a] -> a
product (x:xs) = x * product xs
product []     = 1

reverse :: [a] -> [a]
reverse = go []
  where go rs (x:xs) = go (x:rs) xs
        go rs []     = rs

intersperse :: a-> [a] -> [a]
intersperse x (a:b:bs) = a:x:intersperse x (b:bs)
intersperse _ as       = as

(++) :: [a] -> [a] -> [a]
(++) (x:xs) ys = x:(xs ++ ys)
(++) [] ys     = ys

concat :: [[a]] -> [a]
concat (x:xs) = x ++ concat xs
concat []     = []

tails :: [a] -> [[a]]
tails (x:xs) = (x:xs) : tails xs
tails []     = [[]]

intercalate :: [a] -> [[a]] -> [a]
intercalate xs (as:bs:bss) = as ++ xs ++ intercalate xs (bs:bss)
intercalate _ ass          = concat ass

mapList :: (a -> b) -> [a] -> [b]
mapList _ []     = []
mapList f (x:xs) = f x : mapList f xs

filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList p (x:xs)
  | p x = x : filterList p xs
  | otherwise = filterList p xs

foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList _  acc []     = acc
foldlList op acc (x:xs) = foldlList op (op acc x) xs

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList _  acc []     = acc
foldrList op acc (x:xs) = op x (foldrList op acc xs)

-- Note that those are square brackets, not round brackets.
applyList :: [a -> b] -> [a] -> [b]
applyList [] _      = []
applyList _ []      = []
applyList (f:fs) xs = (f K.<$> xs) ++ applyList fs xs

bindList :: (a -> [b]) -> [a] -> [b]
bindList _ []     = []
bindList f (x:xs) = f x ++ bindList f xs

instance K.Functor [] where
  fmap = mapList

instance K.Applicative [] where
  pure a = [a]
  (<*>) = applyList

instance K.Monad [] where
  (>>=) = flip bindList

transpose :: [[a]] -> [[a]]
transpose (as:ass) = go as (transpose ass)
  where go :: [a] -> [[a]] -> [[a]]
        go (a:as) (bs:bss) = (a:bs):go as bss
        go (a:as) []       = [a]:go as []
        go [] bss          = bss
transpose [] = []
