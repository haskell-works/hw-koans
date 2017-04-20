module Koan.List where

import           Prelude hiding (concat, head, init, last, reverse, tail, (++))

enrolled :: Bool
enrolled = False

head :: [a] -> a
head = error "TODO: implement head"

tail :: [a] -> [a]
tail = error "TODO: implement tail"

last :: [a] -> a
last = error "TODO: implement last"

reverse :: [a] -> [a]
reverse = error "TODO: implement reverse"

(++) :: [a] -> [a] -> [a]
(++) = error "TODO: implement (++)"

concat :: [[a]] -> [a]
concat = error "TODO: implement concat"

tails :: [a] -> [[a]]
tails = error "TODO: implement tails"
