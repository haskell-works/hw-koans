module Lib
    ( countElem
    ) where

{- | Returns a count of the number of times the given element occured in the
given list. -}
countElem :: Eq a => a -> [a] -> Int
countElem i = length . filter (i==)
