module Koan.Maybe where

import           Prelude hiding (Maybe (..))

-- Define a "Maybe" data type.
-- It should represent the concept of there being zero or exactly one instance
-- of a piece of data.
-- In other words, it should represent whether a piece of data is exists or not.

data Maybe a = Undefined

-- This should return the contents of the 'Maybe' if it exists, otherwise
-- it should return the provided value.
orElse :: Maybe a -> a -> a
orElse = error "TODO: Implement orElse"

-- This should return the first Maybe if there's something in it.
-- If not, it should return the second Maybe (regardless of whether its empty
-- or not).
orMaybe :: Maybe a -> Maybe a -> Maybe a
orMaybe = error "TODO: Implement orMaybe"

-- This should apply a function to the contents of a Maybe, if it exists.
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = error "TODO: Implement mapMaybe"

-- Concatenate all the Maybes in the input list, so that you end up with only
-- the ones that have something in them.
concatMaybes :: [Maybe a] -> [a]
concatMaybes = error "TODO: Implement concatMaybes"

-- Filter for Maybes
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe = error "TODO: Implement filterMayber"

-- Fold for Maybes.
foldMaybe :: (b -> a -> b) -> b -> Maybe a -> b
foldMaybe = error "TODO: Implement foldMaybe"

-- Similar to the map function above, but now the function to apply is also in
-- a Maybe.
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe = error "TODO: Implement applyMaybe"

-- Similar to the above map function, but now we must deal with
-- the input function also returning a Maybe.
bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = error "TODO: Implement bindMaybe"
