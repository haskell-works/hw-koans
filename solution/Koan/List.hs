module Koan.List where

import           Prelude hiding (concat, head, init, last, reverse, tail, (++))

enrolled :: Bool
enrolled = False

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

last :: [a] -> a
last (x:[]) = x
last (_:xs) = last xs

reverse :: [a] -> [a]
reverse = go []
  where go rs (x:xs) = go (x:rs) xs
        go rs []     = rs

(++) :: [a] -> [a] -> [a]
(++) (x:xs) ys = x:(xs ++ ys)
(++) [] ys     = ys

concat :: [[a]] -> [a]
concat (x:xs) = x ++ concat xs
concat []     = []

tails :: [a] -> [[a]]
tails (x:xs) = (x:xs) : tails xs
tails []     = [[]]
