module Koan.List where

import           Prelude hiding (concat, head, init, last, reverse, tail, (++))

import           Koan.Functor     as K
import           Koan.Applicative as K
import           Koan.Monad       as K

enrolled :: Bool
enrolled = False

-- Example:
--   head [1, 2, 3, 4] = 1
head :: [a] -> a
head (x:_) = x

-- Example:
--   tail [1, 2, 3, 4] = [2, 3, 4]
tail :: [a] -> [a]
tail (_:xs) = xs

-- Example:
--   last [1, 2, 3, 4] = 4
last :: [a] -> a
last [x] = x
last (_:xs) = last xs

-- Example:
--   reverse [1, 2, 3, 4] = [4, 3, 2, 1]
reverse :: [a] -> [a]
reverse = go []
  where go rs (x:xs) = go (x:rs) xs
        go rs []     = rs

-- Example:
--   [1, 2] ++ [3, 4] = [1, 2, 3, 4]
(++) :: [a] -> [a] -> [a]
(++) (x:xs) ys = x:(xs ++ ys)
(++) [] ys     = ys

-- Example:
--   concat [[1, 2], [3, 4]] = [1, 2, 3, 4]
concat :: [[a]] -> [a]
concat (x:xs) = x ++ concat xs
concat []     = []

-- Example:
--   tails [1, 2, 3] = [[1, 2, 3], [2, 3], [3], []]
tails :: [a] -> [[a]]
tails (x:xs) = (x:xs) : tails xs
tails []     = [[]]

-- Example:
--   mapList show [1, 2, 3, 4] = ["1", "2", "3", "4"]
mapList :: (a -> b) -> [a] -> [b]
mapList _ [] = []
mapList f (x:xs) = f x : mapList f xs

-- Example:
--   filterList even [1, 2, 3, 4] = [2, 4]
filterList :: (a -> Bool) -> [a] -> [a]
filterList _ [] = []
filterList p (x:xs)
  | p x = x : filterList p xs
  | otherwise = filterList p xs

foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList _  acc [] = acc
foldlList op acc (x:xs) = foldlList op (op acc x) xs

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList _  acc []     = acc
foldrList op acc (x:xs) = op x (foldrList op acc xs)

-- Note that those are square brackets, not round brackets.
applyList :: [a -> b] -> [a] -> [b]
applyList [] _ = []
applyList _ [] = []
applyList (f:fs) xs = (f K.<$> xs) ++ applyList fs xs

bindList :: (a -> [b]) -> [a] -> [b]
bindList _ [] = []
bindList f (x:xs) = f x ++ bindList f xs

instance K.Functor [] where
  fmap = mapList

instance K.Applicative [] where
  pure a = [a]
  (<*>) = applyList

instance K.Monad [] where
  (>>=) = flip bindList
