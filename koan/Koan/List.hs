module Koan.List where

import Koan.Applicative as K
import Koan.Functor     as K
import Koan.Monad       as K
import Prelude          hiding (concat, head, init, last, reverse, tail, (++))

enrolled :: Bool
enrolled = False

-- Example:
--   head [1, 2, 3, 4] = 1
head :: [a] -> a
head = error "TODO: implement head"

-- Example:
--   tail [1, 2, 3, 4] = [2, 3, 4]
tail :: [a] -> [a]
tail = error "TODO: implement tail"

-- Example:
--   last [1, 2, 3, 4] = 4
last :: [a] -> a
last = error "TODO: implement last"

-- Example:
--   reverse [1, 2, 3, 4] = [4, 3, 2, 1]
reverse :: [a] -> [a]
reverse = error "TODO: implement reverse"

-- Example:
--   [1, 2] ++ [3, 4] = [1, 2, 3, 4]
(++) :: [a] -> [a] -> [a]
(++) = error "TODO: implement (++)"

-- Example:
--   concat [[1, 2], [3, 4]] = [1, 2, 3, 4]
concat :: [[a]] -> [a]
concat = error "TODO: implement concat"

-- Example:
--   tails [1, 2, 3] = [[1, 2, 3], [2, 3], [3], []]
tails :: [a] -> [[a]]
tails = error "TODO: implement tails"

-- Example:
--   mapList show [1, 2, 3, 4] = ["1", "2", "3", "4"]
mapList :: (a -> b) -> [a] -> [b]
mapList = error "TODO: implement mapList"

-- Example:
--   filterList even [1, 2, 3, 4] = [2, 4]
filterList :: (a -> Bool) -> [a] -> [a]
filterList = error "TODO: implement filterList"

-- Example:
--   foldlList (+) 0 [1, 2, 3] = 6
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
