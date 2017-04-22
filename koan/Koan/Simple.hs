-- These are some simple problems to get your feet wet writing Haskell.
-- They assume you've been introduced to at least the basic syntax of the
-- language.

-- See here if you'd like a basic tour of the syntax:
-- https://github.com/haskell-works/hw-examples/blob/master/src/Examples.hs

-- Do as much as you think is useful. It doesn't matter if you don't complete
-- all of these, because they're all in the standard library (though
-- occasionally with slightly different type signatures.)

module Koan.Simple where

-- This is just hiding some of the standard library, as we're implemeting a lot
-- of it as an exercise.
import           Prelude hiding (all, any, const, curry, drop, dropWhile, elem, filter, flip, foldl, foldr, id, iterate, length, map, max, maximum, min, minimum, repeat, reverse, take, takeWhile, uncurry, zipWith, (!!), ($), (++), (.))

-- There is only a single possible definition of the first two functions.
-- Try to work out what they need to do based on their type signature alone.
id :: a -> a
id = error "TODO: Implement id"

const :: a -> b -> a
const = error "TODO: Implement const"

-- This is function composition, i.e. (f . g)(x) == f(g(x))
(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = error "TODO: Implement (.)"

-- This flips the argument order of a function, i.e.
-- f(a,b) == (flip f)(b,a)
flip :: (a -> b -> c) -> b -> a -> c
flip = error "TODO: Implement flip"

-- This is function application (i.e. calling a function), but with a lower
-- precedence than normal
($) :: (a -> b) -> a -> b
($) = error "TODO: Implement ($)"

-- Precedence definition for ($). You don't need to do anything here.
infixr 0 $

--------------------------------------------------------------------------------
-- LISTS
--------------------------------------------------------------------------------

-- These are some simple operations on lists.

-- Return the length of a list
length :: [a] -> Int
length = error "TODO: Implement length"

-- This returns the element at a specific index in a list (if it exists),
-- or nothing otherwise.
-- You'll probably need to use pattern matching.
(!!) :: [a] -> Int -> Maybe a
(!!) = error "TODO: Implement (!!)"

-- ASIDE: In the standard library, (!!) has the type `[a] -> Int -> a`
-- meaning it fails (crashes the program) if the index you request does not
-- exist. Returning `Maybe a` is the safer way to implement it.

-- This concatenates two lists together.
-- Again, you'll probably need to use pattern matching.
(++) :: [a] -> [a] -> [a]
(++) = error "TODO: Implement (++)"

-- Reverse a list.
-- Note that while you can get a "correct" solution using (++), it will be very
-- inefficient. Try to implement it without that. You may need a helper
-- function or a sub function.
reverse :: [a] -> [a]
reverse = error "TODO: Implement reverse"

--------------------------------------------------------------------------------
-- Infinite lists
--------------------------------------------------------------------------------

-- These functions should happily work on infinite lists.

-- repeat the input forever.
repeat :: a -> [a]
repeat = error "TODO: Implement repeat"

-- Return a list of the result of repeatedly applying f to x
-- i.e iterate f x = [x, f x, f (f x), ...]
iterate :: (a -> a) -> a -> [a]
iterate = error "TODO: Implement iterate"

-- Returns the first `n` elements of a list.
-- If there are less than `n` elements, just return all of them.
take :: Int -> [a] -> [a]
take = error "TODO: Implement take"

-- Similar to the above, but drop the first `n` elements and return the rest.
-- If there are less than `n` elements, return an empty list.
drop :: Int -> [a] -> [a]
drop = error "TODO: Implement drop"

-- Take as long as the elements in the list satisfy the given predicate.
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile = error "TODO: Implement takeWhile"

-- You get the idea.
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = error "TODO: Implement dropWhile"

--------------------------------------------------------------------------------
-- Higher order functions on lists
--------------------------------------------------------------------------------

-- These are the classic map, filter, and fold (a.k.a reduce) for lists.

map :: (a -> b) -> [a] -> [b]
map = error "TODO: Implement map"

filter :: (a -> Bool) -> [a] -> [a]
filter = error "TODO: Implement filter"

-- The 'l' in `foldl` indicates that it is left-associative.
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl = error "TODO: Implement fold"

-- Similarly, the 'r' indicates `foldr` is right associative.
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = error "TODO: Implement foldr"

-- Functions which tell you whether any or all of the elements of a list
-- satisfy a given predicate.
any :: (a -> Bool) -> [a] -> Bool
any = error "TODO: Implement any"

all :: (a -> Bool) -> [a] -> Bool
all = error "TODO: Implement all"

-- This should take a binary (i.e. two argument) function, and two lists, and
-- "zip" the elements of the list together pairwise using that function.
-- If one input is shorter than the other, it should discard the excess
-- elements of the longer list.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith = error "TODO: Implement zipWith"

--------------------------------------------------------------------------------
-- Currying
--------------------------------------------------------------------------------

-- Turn an uncurried function (i.e. one that takes a tuple as its argument)
-- and curry it (i.e. make it take its arguments one-by-one)
curry :: ((a, b) -> c) -> a -> b -> c
curry = error "TODO: Implement curry"

-- The inverse of the above.
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry = error "TODO: Implement uncurry"

--------------------------------------------------------------------------------
-- Functions require Equality
--------------------------------------------------------------------------------

-- Determines if an element is in a list
elem :: Eq a => a -> [a] -> Bool
elem = error "TODO: Implement elem"

--------------------------------------------------------------------------------
-- Functions require Ordering
--------------------------------------------------------------------------------

-- Take to guess what these should do!

max :: Ord a => a -> a -> a
max = error "TODO: Implement max"

min :: Ord a => a -> a -> a
min = error "TODO: Implement min"

-- Note: These can fail if the list is empty.
-- We'll alow that in thise case. You can use the `error` function to return
-- an error.
maximum :: Ord a => [a] -> a
maximum = error "TODO: Implement maximum"

minimum :: Ord a => [a] -> a
minimum = error "TODO: Implement minimum"

--------------------------------------------------------------------------------
-- Miscellaneous Exercises
--------------------------------------------------------------------------------

-- Define a list of all fibonacci numbers.
-- (Hint: Try to use the `zipWith` function defined above)
fibonacci :: [Int]
fibonacci = error "TODO: Implement fibonacci"
