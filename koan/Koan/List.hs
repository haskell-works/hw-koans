module Koan.List where

import           Prelude hiding (concat, head, init, last, reverse, tail, (++))
import           Koan.Functor     as K
import           Koan.Applicative as K
import           Koan.Monad       as K


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

mapList :: (a -> b) -> [a] -> [b]
mapList = error "TODO: implement mapList"

filterList :: (a -> Bool) -> [a] -> [a]
filterList = error "TODO: implement filterList"

foldlList :: (b -> a -> b) -> b -> [a] -> b
foldlList = error "TODO: implement foldlList"

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList = error "TODO: implement foldlList"

-- Note that those are square brackets, not round brackets.
applyList :: [a -> b] -> [a] -> [b]
applyList = error "TODO: implement applyList"

bindList :: (a -> [b]) -> [a] -> [b]
bindList = error "TODO: implement bindList"

instance K.Functor [] where
  fmap = error "TODO: Implement fmap for ([a])"

instance K.Applicative [] where
  pure = error "TODO: Implement Applicative pure for []"
  (<*>) = error "TODO: Implement Applicative (<*>) for []"

instance K.Monad [] where
  (>>=) = error "TODO: Implement Monad (>>=) for []"
